%%% Copyright (c) 2012 Eric des Courtis <eric.des.courtis@gmail.com>
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


-module(gsm0338_spanish).
-behaviour(gen_gsm0338).

-export([gsm/1, cp/1, cp_ext/1, to_utf8/1, from_utf8/1]).

to_utf8(Binary) ->
    gen_gsm0338:to_utf8(?MODULE, Binary).

from_utf8(UTF8) ->
    gen_gsm0338:from_utf8(?MODULE, UTF8).

gsm(16#00C1) -> 16#1B41;
gsm(16#00E1) -> 16#1B61;
gsm(16#00DA) -> 16#1B55;
gsm(16#00FA) -> 16#1B75;
gsm(16#00E7) -> 16#1B09;
gsm(16#00CD) -> 16#1B49;
gsm(16#00ED) -> 16#1B69;
gsm(16#00D3) -> 16#1B4F;
gsm(16#00F3) -> 16#1B6F;
gsm(CP)       -> gsm0338_basic:gsm(CP).

cp(GSM) -> gsm0338_basic:cp(GSM).

cp_ext(16#41) -> 16#00C1;
cp_ext(16#61) -> 16#00E1;
cp_ext(16#55) -> 16#00DA;
cp_ext(16#75) -> 16#00FA;
cp_ext(16#09) -> 16#00E7;
cp_ext(16#49) -> 16#00CD;
cp_ext(16#69) -> 16#00ED;
cp_ext(16#4F) -> 16#00D3;
cp_ext(16#6F) -> 16#00F3;
cp_ext(GSM)   -> gsm0338_basic:cp_ext(GSM).

