%%% Copyright (c) 2011 Aleksey Yeschenko <aleksey@yeschenko.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(gen_gsm0338).

-export([behaviour_info/1]).

-export([from_utf8/2, to_utf8/2]).

behaviour_info(callbacks) ->
    [{cp, 1}, {cp_ext, 1}, {gsm, 1}, {to_utf8, 1}, {from_utf8, 1}];
behaviour_info(_) ->
    undefined.


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec from_utf8(atom(), binary()) -> {invalid, binary()} | {valid, binary()} | {error, binary(), RestData :: binary()}
                           | {incomplete, binary(), IncompleteSeq :: binary()}.
from_utf8(Module, UTF8) when is_atom(Module) ->
    case unicode:characters_to_list(UTF8, utf8) of
        CodePoints when is_list(CodePoints) ->
            codepoints_to_gsm(Module, CodePoints);
        {error, CodePoints, RestData} ->
            {error, codepoints_to_gsm(Module, CodePoints), RestData};
        {incomplete, CodePoints, IncompleteSeq} ->
            {incomplete, codepoints_to_gsm(Module, CodePoints), IncompleteSeq}
    end.

-spec to_utf8(atom(), binary()) -> binary() | {error, binary(), RestData :: binary()}.
to_utf8(Module, GSM) ->
    gsm_to_codepoints(Module, GSM, []).

%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------

codepoints_to_gsm(Module, CodePoints) ->
	codepoints_to_gsm(Module, CodePoints, [], valid).
codepoints_to_gsm(_Module, [], Acc, Validity) ->
    {Validity, list_to_binary(lists:reverse(Acc))};
codepoints_to_gsm(Module, [CP|Rest], Acc, Validity) ->
    case Module:gsm(CP) of
        undefined ->
            codepoints_to_gsm(Module, Rest, [16#3F|Acc],invalid);
        N when N =< 16#FF ->
            codepoints_to_gsm(Module, Rest, [N|Acc], Validity);
        N ->
            codepoints_to_gsm(Module, Rest, [N rem 256,N div 256|Acc], Validity)
    end.

gsm_to_codepoints(_Module, <<>>, Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc), utf8);
gsm_to_codepoints(Module, <<C, _/binary>> = GSM, Acc) when C > 16#7F ->
    {error, gsm_to_codepoints(Module, <<>>, Acc), GSM};
gsm_to_codepoints(Module, <<16#1B, Ext, Rest/binary>>, Acc) ->
    case Module:cp_ext(Ext) of
        undefined ->
            gsm_to_codepoints(Module, <<Ext, Rest/binary>>, [Module:cp(16#1B)|Acc]);
        CP ->
            gsm_to_codepoints(Module, Rest, [CP|Acc])
    end;
gsm_to_codepoints(Module, <<C, Rest/binary>>, Acc) ->
    gsm_to_codepoints(Module, Rest, [Module:cp(C)|Acc]).


