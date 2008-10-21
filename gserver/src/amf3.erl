% This module supports decoding from and encoding to AMF3. Types supported:
% * undefined, null
% * false, true
% * integer, number
% * string
% * dense array
%
% More information:
% * Official AMF3 spec: http://download.macromedia.com/pub/labs/amf/amf3_spec_121207.pdf
% * Reverse Engineered spec: http://osflash.org/documentation/amf3

-module(amf3).
-export([decode/1, encode/1]).

%-------------------------------------------------------------------------------
-define(UNDEFINED, 0).
-define(NULL,      1).
-define(FALSE,     2).
-define(TRUE,      3).
-define(INTEGER,   4).
-define(NUMBER,    5).
-define(STRING,    6).
-define(ARRAY,     9).

%-------------------------------------------------------------------------------
decode(<<>>) ->
    [];
decode(<<?UNDEFINED, Rest/binary>>) ->
    [undefined | decode(Rest)];
decode(<<?NULL, Rest/binary>>) ->
    [null | decode(Rest)];
decode(<<?FALSE, Rest/binary>>) ->
    [false | decode(Rest)];
decode(<<?TRUE, Rest/binary>>) ->
    [true | decode(Rest)];
decode(<<?INTEGER, Rest/binary>>) ->
    {Result, Rest2} = from_amf3_integer(Rest),
    [Result | decode(Rest2)];
decode(<<?NUMBER, Rest/binary>>) ->
    <<Float:64/float, Rest2/binary>> = Rest,
    [Float | decode(Rest2)];
decode(<<?STRING, Len:16/unsigned, Rest/binary>>) ->
    <<String:Len/binary, Rest2/binary>> = Rest,
    [binary_to_list(String), decode(Rest2)].
%decode(<<?ARRAY

%-------------------------------------------------------------------------------
from_amf3_integer(Data) ->
    from_amf3_integer(Data, 0, 0).

from_amf3_integer(<<1:1, Num:7, Rest/binary>>, Result, N) when N < 3 ->
    from_amf3_integer(Rest, (Result bsl 7) bor Num, N + 1);
from_amf3_integer(<<0:1, Num:7, Rest/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Rest};
%TODO:
%1100 0001 1111 1111 1111 1111 1111 1111 = -268435456
%1100 0000 1000 0001 1000 0001 1000 0000 = 268435455
from_amf3_integer(<<Byte, Rest/binary>>, Result, _N) ->
    Result1 = (Result bsl 8) bor Byte,
    Result3 = case Result1 band 16#10000000 of
      16#10000000 ->
          Extended = Result1 bor 16#e0000000,
          <<Result2:32/signed>> = <<Extended:32>>,
          Result2;
      0 ->
          Result1
    end,
    {Result3, Rest}.

%-------------------------------------------------------------------------------
encode(Variable) ->
    ok.
