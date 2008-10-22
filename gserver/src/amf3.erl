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
-export([decode_all/1, encode/1]).

%-------------------------------------------------------------------------------
-define(UNDEFINED,   0).
-define(NULL,        1).
-define(FALSE,       2).
-define(TRUE,        3).
-define(INTEGER,     4).
-define(NUMBER,      5).
-define(STRING,      6).
-define(ARRAY,       9).
-define(BYTE_ARRAY, 12).

%-------------------------------------------------------------------------------
decode_all(Data) ->
    decode_all(Data, []).

decode_all(<<>>, Acc) ->
    lists:reverse(Acc);
decode_all(Data, Acc) ->
    {Result, Rest} = decode(Data),
    decode_all(Rest, [Result | Acc]).

%-------------------------------------------------------------------------------
decode(<<?UNDEFINED, Rest/binary>>) ->
    {undefined, Rest};

decode(<<?NULL, Rest/binary>>) ->
    {null, Rest};

decode(<<?FALSE, Rest/binary>>) ->
    {false, Rest};

decode(<<?TRUE, Rest/binary>>) ->
    {true, Rest};

decode(<<?INTEGER, Rest/binary>>) ->
    decode_integer(Rest);

decode(<<?NUMBER, Rest/binary>>) ->
    <<Float:64/float, Rest2/binary>> = Rest,
    {Float, Rest2};

decode(<<?STRING, Rest/binary>>) ->
    {Type, Rest2} = decode_integer(Rest),
    Length = Type bsr 1,
    <<String:Length/binary, Rest3/binary>> = Rest2,
    {binary_to_list(String), Rest3};

decode(<<?ARRAY, Rest/binary>>) ->
    {Type, Rest2} = decode_integer(Rest),
    Length = Type bsr 1,
    decode_array(Length, Rest2).

%-------------------------------------------------------------------------------
decode_integer(Data) ->
    decode_integer(Data, 0, 0).

decode_integer(<<1:1, Num:7, Rest/binary>>, Result, N) when N < 3 ->
    decode_integer(Rest, (Result bsl 7) bor Num, N + 1);
decode_integer(<<0:1, Num:7, Rest/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Rest};
decode_integer(<<Byte, Rest/binary>>, Result, _N) ->
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
decode_array(Length, <<1, Data/binary>>) ->
    decode_array(Length, Data, []).

decode_array(0, Data, Acc) ->
    {lists:reverse(Acc), Data};
decode_array(Length, Data, Acc) ->
    {Item, Rest} = decode(Data),
    decode_array(Length - 1, Rest, [Item | Acc]).

%-------------------------------------------------------------------------------
encode(Variable) ->
    ok.
