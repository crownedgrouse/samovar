%%%-------------------------------------------------------------------
%%% File:      samovar.erl
%%% @author    Eric Pailleau <samovar@crownedgrouse.com>
%%% @copyright 2019 crownedgrouse.com
%%% @doc
%%%     semver library for Erlang
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2019-09-08
%%%-------------------------------------------------------------------
-module(samovar).
-author("Eric Pailleau <samovar@crownedgrouse.com>").

-dialyzer([{nowarn_function, [strip_range/1]}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([simple_range/1, parse_range/1]).
-export([check/2]).
-export([parse/1]).
-export([proplist/1]).
-export([versionize/1]).
-export([major/1, minor/1, patch/1, suffix/1, prerelease/1, build/1]).
-export([version/0]).

%% semver regex
-define(SEMVER, "(\\*|x|[^0-9]{0,})([0-9\\*x]+){0,1}(\.[0-9\\*x]+){0,1}(\\.[0-9x]+){0,1}([\\-|\\+].*){0,}").

-include("samovar.hrl").

-opaque version() :: #version{}.

-export_type([version/0]).

%%-------------------------------------------------------------------------
%% @doc  Export version type
%% @end
%%-------------------------------------------------------------------------
-spec version() -> version().

version() -> #version{}.

%%-------------------------------------------------------------------------
%% @doc  Check a version against a semver range
%% @end
%%-------------------------------------------------------------------------
-spec check(string() | tuple(), string() | tuple()) -> boolean() | {error, atom()}.

check(Version, Range)
    when is_list(Version), is_list(Range) ->
    try
      L = 
        case parse(Version) of
          {ok, V}  -> V ;
          {error, Reason1} -> throw(Reason1)
        end,
      R = parse_range(Range),
      %erlang:display({L, R}),
      check(L, R)
    catch 
      _:Reason -> {error, Reason}
    end;
check(Version, {'and', L, R})
   when is_record(Version, version) ->
   (check(Version, L) and check(Version, R));
check(Version, {'or', L, R})
   when is_record(Version, version) ->
   (check(Version, L) or check(Version, R));
check(_Version, {_, lowest})  -> true;
check(_Version, {_, highest}) -> true;
check(Version, {Comp}) -> check(Version, Comp, [], [], []);
check(Version, {Comp, Major}) -> check(Version, Comp, Major, [], []);
check(Version, {Comp, Major, Minor}) -> check(Version, Comp, Major, Minor, []);
check(Version, {Comp, Major, Minor, Patche}) -> check(Version, Comp, Major, Minor, Patche).


check(Version, Comp, Major, Minor, Patche)
   when (Comp==[]) -> check(Version, "=", Major, Minor, Patche);
check(Version, Comp, Major, Minor, Patche)
   when (Major==[]) -> check(Version, Comp, "0", Minor, Patche);
check(Version, Comp, Major, Minor, Patche)
   when (Minor==[]) -> check(Version, Comp, Major, "0", Patche);
check(Version, Comp, Major, Minor, Patche) 
   when (Patche==[]) -> check(Version, Comp, Major, Minor, "0");
check(Version, Comp, Major, Minor, Patche)
   when is_record(Version, version), is_list(Comp), is_list(Major), is_list(Minor), is_list(Patche) ->

   C = case (Version#version.major == Major) of
        false -> 
          case (safe_list_to_integer(Version#version.major) > safe_list_to_integer(Major)) of
             true  -> ">" ;
             false -> "<"
          end;
        true   -> 
          case ((Version#version.minor == Minor)
                  or 
               (Version#version.minor == []) and (Minor == "0")) of
                   false -> 
                    case (safe_list_to_integer(Version#version.minor) > safe_list_to_integer(Minor)) of
                          true  -> ">" ;
                          false -> "<"
                    end;
                   true  -> 
                      case ((Version#version.patch == Patche)
                            or 
                            (Version#version.patch == []) and (Patche == "0")) of
                         true  -> "=" ;
                         false -> case (safe_list_to_integer(Version#version.patch) > safe_list_to_integer(Patche)) of
                                        true  -> ">" ;
                                        false -> "<"
                                  end
                      end
         end
      end,
   case Comp of
        "="  when (C == "=") -> true ;
        ">=" when (C == "=") -> true ;
        "<=" when (C == "=") -> true ;
        ">=" when (C == ">") -> true ;
        ">"  when (C == ">") -> true ;
        "<=" when (C == "<") -> true ;
        "<"  when (C == "<") -> true ;
        _ -> false
   end.
%%-------------------------------------------------------------------------
%% @doc Translate old release name into new name
%%      Exemple : R16B03 into 16.2.3
%% @end
%%-------------------------------------------------------------------------
-spec versionize(list()) -> list().

versionize([]) -> [];
versionize(V)  ->
   [Pref | Tail] = V,
   case Pref of
      $R -> Split = re:split(Tail,"([AB])",[{return,list}]),
            Fun = fun(X) -> 
                     XX = case X of
                        "A" -> "1" ;
                        "B" -> "2" ;
                        X   -> X
                     end,
                     case string:to_integer(XX) of
                        {error, Z} when is_atom(Z) -> [] ;
                        {L, []} -> [integer_to_list(L)] ;
                        {L, R} when is_integer(L),
                                    is_list(R) -> [integer_to_list(L) ++ R] ;
                        _                   -> []
                     end
               end,
               New_ = lists:map(Fun, Split),
               New  = lists:filter(fun(N) -> case N of [] -> false; _ -> true end end, New_),
               VV   = lists:flatten(string:join(New, ".")),
               VV;
      $>  -> [Pref] ++ versionize(Tail);
      $<  -> [Pref] ++ versionize(Tail);
      $=  -> [Pref] ++ versionize(Tail);
      $~  -> [Pref] ++ versionize(Tail);
      _   -> V
   end.
%%-------------------------------------------------------------------------
%% @doc  Parse semver string
%% @end
%%-------------------------------------------------------------------------
-spec parse(string()) -> {ok, tuple()} | {error, atom()}.

parse(V) when is_list(V) ->
  try
    case re:run(versionize(V), ?SEMVER, [global, {capture, all_but_first, list}, notempty]) of
       {match, Captured} -> C = case Captured of
                                 [["*",[],[]],[[],[],[]]] -> {"*", "", "", "", ""};
                                 [["*"]]                  -> {"*", "", "", "", ""};
                                 [["x"]]                  -> {"x", "", "", "", ""};
                                 [[Co, Maj]]              -> {Co, Maj, "", "", ""};
                                 [[Co, Maj, Min]]         -> {Co, Maj, supdot(Min), "", ""};
                                 [[Co, Maj, Min, Pat]]    -> {Co, Maj, supdot(Min), supdot(Pat), ""};
                                 [[Co, Maj, Min, Pat, P]] -> {Co, Maj, supdot(Min), supdot(Pat), P};
                                 _  -> throw({parse_error, Captured}),
                                       {"", "", "", "", ""}
                                end,
                            {Comp, Major, Minor, Patch, Suffix_} = C,
                            {Suffix, Pre, Build} = split_suffix(Suffix_),
                            {ok, #version{comp = Comp
                                         , major = Major
                                         , minor = Minor
                                         , patch = Patch
                                         , suffix = Suffix
                                         , pre = Pre
                                         , build = Build
                                         }};
       match             -> throw(match); % Should never happen !
       nomatch           -> throw(nomatch);
       {error, ErrType}  -> throw(ErrType)
    end
  catch
      _:_E -> %erlang:display(_E),
        {error, invalid_version}
  end.

%%-------------------------------------------------------------------------
%% @doc  Get a version in proplist elements
%% @end
%%-------------------------------------------------------------------------
-spec proplist(string()) -> {ok, list()} | {error, atom()}.

proplist(V) 
  when is_list(V)
  -> 
    case parse(V) of
      {error, X} -> {error, X};
      {ok, R} when is_record(R, version) 
              -> {ok, record_to_proplist(R)}
    end.

%%-------------------------------------------------------------------------
%% @doc  Get major
%% @end
%%-------------------------------------------------------------------------
major(V) -> 
  case parse(V) of
    {ok, R} -> safe_list_to_integer(R#version.major);
    E -> E
  end.
%%-------------------------------------------------------------------------
%% @doc  Get minor
%% @end
%%-------------------------------------------------------------------------
minor(V) ->
  case parse(V) of
    {ok, R} -> safe_list_to_integer(R#version.minor);
    E -> E
  end.
%%-------------------------------------------------------------------------
%% @doc  Get patch
%% @end
%%-------------------------------------------------------------------------
patch(V) ->
  case parse(V) of
    {ok, R} -> safe_list_to_integer(R#version.patch);
    E -> E
  end.
%%-------------------------------------------------------------------------
%% @doc  Get suffix
%% @end
%%-------------------------------------------------------------------------
suffix(V) ->
  case parse(V) of
    {ok, R} -> R#version.suffix;
    E -> E
  end.
%%-------------------------------------------------------------------------
%% @doc  Get prerelease
%% @end
%%-------------------------------------------------------------------------
prerelease(V) ->
  case parse(V) of
    {ok, R} -> R#version.pre;
    E -> E
  end.
%%-------------------------------------------------------------------------
%% @doc  Get buildmetadata
%% @end
%%-------------------------------------------------------------------------
build(V) ->   
  case parse(V) of
    {ok, R} -> R#version.build;
    E -> E
  end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            Local functions                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%-------------------------------------------------------------------------
%% @doc  Extract simple range
%% @end
%%-------------------------------------------------------------------------
-spec simple_range(list()) -> tuple().

simple_range(X) -> 
   case catch parse(X) of
      {error, E} -> 
        {error, E};
      {ok, M}    -> 
        strip_range(simple_range_translate(M))
   end.

%%-------------------------------------------------------------------------
%% @doc Translate simple range
%% @end
%%-------------------------------------------------------------------------
-spec simple_range_translate(tuple()) -> {ok, tuple() | atom(), tuple() | atom()} | {error, atom()}.

simple_range_translate(X) 
   when is_record(X, version) -> 
   simple_range_translate(X, X#version.comp).

simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major == "") -> 
   simple_range_translate(#version{major = "0", minor = Minor, patch = Patch}, C);
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major =/= ""),(Minor == "") -> 
   simple_range_translate(#version{major = Major, minor = "0", patch = Patch}, C);
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major =/= ""),(Minor =/= ""),(Patch == "") -> 
   simple_range_translate(#version{major = Major, minor = Minor, patch = "0"}, C);
simple_range_translate(#version{major = Major, minor = _Minor, patch = _Patch}, _)
   when (Major == "x");(Major == "*") -> 
   simple_range_translate(#version{major = "0", minor = "0", patch = "0"}, ">=");
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, _)
   when (Minor == "x");(Minor == "*") -> 
   parse_range(io_lib:format(">=~ts.0.0 <~ts.0.0", [Major, erlang:integer_to_list(erlang:list_to_integer(Major) + 1)]));
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, _)
   when (Patch == "x");(Patch == "*") -> 
   parse_range(io_lib:format(">=~ts.~ts.0 <~ts.~ts.0", [Major, Minor, Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1)]));
% ~ 	means “reasonably close to”
% ~1.2.3 	is >=1.2.3 <1.3.0
% ~1.2 	   is >=1.2.0 <1.3.0 	(like ~1.2.0)
% ~1 	     
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "\~")
   when (Major =/= ""),(Minor =/= ""),(Minor =/= "0"),(Patch =/= ""),(Patch =/= "0") -> 
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor}, "\~")
   when (Major =/= ""),(Minor =/= ""),(Minor =/= "0") ->
   Min = {">=",Major, Minor, "0"},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major}, "\~")
   when (Major =/= "") -> 
   Min = {">=",Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
% * 	      any version
simple_range_translate(#version{major = _Major, minor = _Minor, patch = _Patch}, "*")
   -> {ok, {">=", lowest}, {"<=", highest}};
simple_range_translate(#version{major = _Major, minor = _Minor, patch = _Patch}, "x")
   -> {ok, {">=", lowest}, {"<=", highest}};

% ^ 	means “compatible with”
% ^1.2.3 	is >=1.2.3 <2.0.0
% ^0.2.3 	is >=0.2.3 <0.3.0 	(0.x.x is special)
% ^0.0.1 	is =0.0.1 	         (0.0.x is special)
% ^1.2 	   is >=1.2.0 <2.0.0 	(like ^1.2.0)
% ^1 	      is >=1.0.0 <2.0.0
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") -> 
   Min = {">=", Major, Minor, Patch},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor =/= "0"),(Patch =/= "") -> 
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor == "0"),(Patch =/= "") -> 
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Minor =/= ""),(Patch == "") -> 
   Min = {">=", Major, Minor, "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major}, "^")
   when (Major =/= "") -> 
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
%% = >= > <
%simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "")
%   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") -> 
%   Min = {"=", Major, Minor, Patch},
%   Max = {"=", Major, Minor, Patch},
%   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "=")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") -> 
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, ">=")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") -> 
   Min = {">=", Major, Minor, Patch},
   Max = {"<=", highest},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, ">")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") -> 
   Min = {">", Major, Minor, Patch},
   Max = {"<=", highest},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "<=")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") -> 
   Min = {">=",lowest},
   Max = {"<=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "<")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") -> 
   Min = {">=",lowest},
   Max = {"<", Major, Minor, Patch},
   {ok, Min, Max};
% 1.x 	   same
% 1.* 	   same
% 1 	      same
% x 	      same
simple_range_translate(#version{major = Major, minor = _Minor, patch = _Patch}, "")
   when (Major == "*");(Major == "x") -> 
   Min = {">=", lowest},
   Max = {"<=", highest},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),((Minor == "x") or (Minor == "*")) -> 
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),(Minor == "") -> 
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),(Minor =/= ""),(Minor =/= "0") -> 
   Min = {">=", Major, "0", "0"},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),(Minor =/= "") -> 
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(_X, _Y)
   -> {error, invalid_range}.

%%-------------------------------------------------------------------------
%% @doc Parse range into 'and' and 'or' logic
%% @end
%%-------------------------------------------------------------------------
% Hyphenated ranges
% 1.2.3 - 2.3.0 	is >=1.2.3 <=2.3.0
% Partial right
% 1.2.3 - 2.3 	is >=1.2.3 <2.4.0
% 1.2.3 - 2 	is >=1.2.3 <3.0.0
% Partial left
% 1.2 - 2.3.0 	is 1.2.0 - 2.3.0
% When the right is partial (eg, 2.3), missing pieces are assumed to be x (eg, 2.3.x).
% When the left is partial (eg, 1.2), missing pieces are assumed to be 0 (eg, 1.2.0).
% Combining ranges
% >=0.14 <16 	      And (space-separated)
% 0.14.x || 15.x.x 	Or (pipe-separated)
-spec parse_range(list()) -> tuple() | no_return().

parse_range(V) when is_list(V) -> 
  try
   case string:tokens(V, "||") of 
     [Left | T] when (T =/= []) -> 
         {'or', parse_range(trim(Left)), parse_range(lists:flatten(lists:join("||", T)))} ;
     [V] -> 
       case string:tokens(V, "-") of
          [Left_, Right_]   
            -> 
               Left  = rtrim(Left_),
               Right = ltrim(Right_),
               {ok, #version{major = Major, minor = Minor, patch = Patch} = X} = parse(Right),
               case X of
                   X when (Patch =/= "")
                      -> parse_range(io_lib:format(">=~ts <=~ts", [Left, Right]));
                   % Partial right
                   X when (Minor == ""),(Patch == "")
                      -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"]));
                   X when (Patch == "")
                      -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"]))

               end ;
          [V] -> 
               case string:tokens(V, " ") of
                  [Left, Right] -> 
                      {ok, L1, R1} = simple_range(rtrim(Left)),
                      {ok, L2, R2} = simple_range(ltrim(Right)),
                      {'and', {'and', L1, R1}, {'and', L2, R2}};
                  [Single] -> 
                      {ok, L1, R1} = simple_range(trim(Single)),
                      {'and', L1, R1}
               end
          end
      end
  catch 
    _:_E ->  
      %erlang:display({error, _E}), 
      throw(invalid_range)
  end.

%%-------------------------------------------------------------------------
%% @doc Remove first dot
%% @end
%%-------------------------------------------------------------------------
-spec supdot(string()) -> string().

supdot(S) -> supchar(S, $.).

%%-------------------------------------------------------------------------
%% @doc Remove first dash
%% @end
%%-------------------------------------------------------------------------
%-spec supdash(string()) -> string().

%supdash(S) -> supchar(S, $-).

%%-------------------------------------------------------------------------
%% @doc Remove first given char
%% @end
%%-------------------------------------------------------------------------
-spec supchar(string(), char()) -> string().

supchar(S, C) -> case S of
                     [C | Rest] -> Rest ;
                     _ -> S
                 end.

%%-------------------------------------------------------------------------
%% @doc trim functions
%% @end
%%-------------------------------------------------------------------------
ltrim([32  | T]) -> ltrim(T) ;
ltrim(X) -> X.

rtrim(X) -> lists:reverse(ltrim(lists:reverse(X))).

trim(X) -> rtrim(ltrim(X)).

%%-------------------------------------------------------------------------
%% @doc join function
%%      string:join is available since 19.0 only
%% @end
%%-------------------------------------------------------------------------
% -spec join(Sep, List1) -> List2 when
%       Sep :: T,
%       List1 :: [T],
%       List2 :: [T],
%       T :: term().

% join(_Sep, []) -> [];
% join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

% join_prepend(_Sep, []) -> [];
% join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

%%-------------------------------------------------------------------------
%% @doc Safe conversion of list to integer
%%      Empty list considered to be 0
%% @end
%%-------------------------------------------------------------------------
-spec safe_list_to_integer(string()) -> integer().

safe_list_to_integer([]) -> safe_list_to_integer("0");
safe_list_to_integer(L) -> erlang:list_to_integer(L).

%%-------------------------------------------------------------------------
%% @doc Convert version record to proplist
%% @end
%%-------------------------------------------------------------------------
record_to_proplist(#version{} = Rec) ->
  lists:zip(record_info(fields, version), tl(tuple_to_list(Rec))).

%%-------------------------------------------------------------------------
%% @doc Split version suffix
%% @end
%%-------------------------------------------------------------------------
split_suffix(S)
  when is_list(S)
  -> 
  Suff =
    case S of
      [$- | S1] -> S1 ;
      [$+ | S2] -> S2 ;
      S3 -> S3
    end,
  L  = lists:filter(fun(X) -> case X of [] -> false ; _ -> true end end,
                    re:split(S,"([\+\-])",[{return,list}])),
  Pre = case L of
          ["-", Pre1 | _] -> Pre1 ;
          ["+", _] -> [] ;
          _ -> []
        end,
  Build = case L of
          ["-", _, "+", Build1] -> Build1 ;
          ["+", Build2] -> Build2 ;
          _ -> []
        end,
  {Suff, Pre, Build}.

%%-------------------------------------------------------------------------
%% @doc Strip range
%% @end
%%-------------------------------------------------------------------------
-spec strip_range(tuple()) -> {ok, tuple() | atom(), tuple() | atom()} | {error, atom()}.

strip_range({ok, L, R})    
  -> %erlang:display({ok, L, R}), 
  {ok, L, R};
strip_range({'and', L, R}) 
  -> %erlang:display({ok, L, R}), 
  {ok, L, R};
strip_range({'error', E}) 
  -> %erlang:display({ok, L, R}), 
  {'error', E}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               TESTS                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-ifdef(TEST).

parse_test() ->
   ?assertEqual({ok,{version, [], "16", "2", "3", "1","1",[]}}, parse("R16B03-1"))
  ,?assertEqual({ok,{version, [], "16", "2", "2", "", "", ""}}, parse("R16B02"))
  ,?assertEqual({ok,{version, [], "16", "2", "", "", "", ""}}, parse("R16B"))
  .

parse_range_test() ->
    ?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","0"}}},
                 parse_range("1.2.3 - 2.3.0"))
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","4"}}},
                 parse_range("1.2.3 - 2.3.4"))
   % Partial right
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","2","4","0"}}},
                 parse_range("1.2.3 - 2.3"))
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","3","0","0"}}},
                 parse_range("1.2.3 - 2"))
   % Partial left
   ,?assertEqual({'and',{'and',{">=","1","2","0"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","0"}}},
                 parse_range("1.2 - 2.3.0"))
   % Combining ranges
   ,?assertEqual({'and',{'and',{">=","0","14","0"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","16","0","0"}}},
                 parse_range(">=0.14 <16"))
   ,?assertEqual({'or', {'and',{'and',{">=","0","14","0"},{"<=",highest}},
                               {'and',{">=",lowest},{"<","0","15","0"}}},
                        {'and',{'and',{">=","15","0","0"},{"<=",highest}},
                               {'and',{">=",lowest},{"<","16","0","0"}}}},
                 parse_range("0.14.x || 15.x.x"))
   ,?assertEqual({'or',{'and',{">=",lowest},{"<","11","0","0"}},
                       {'and',{">","13","0","0"},{"<=",highest}}},
                 parse_range("<11 || >13"))
   ,?assertEqual({'or', {'and',{">=",lowest},{"<","11","0","0"}},
                        {'or',{'and',{">","13","0","0"},{"<=",highest}},
                              {'and',{'and',{">=","12","3","0"},{"<=",highest}},
                                     {'and',{">=",lowest},{"<","12","4","0"}}}}},
                 parse_range("<11 || >13 || 12.3.x"))
   ,ok.


check_test() ->
    %   1.2.3
    ?assertEqual(true, check("1.2.3","1.2.3"))
    %  =1.2.3
   ,?assertEqual(true, check("1.2.3","=1.2.3"))
   ,?assertEqual(false, check("1.2.4","=1.2.3"))
   ,?assertEqual(false, check("1.3.2","=1.2.3"))
   ,?assertEqual(false, check("3.2.1","=1.2.3"))
    %  >1.2.3
   ,?assertEqual(true, check("1.2.4",">1.2.3"))
   ,?assertEqual(true, check("1.4.3",">1.2.3"))
   ,?assertEqual(true, check("2.3.1",">1.2.3"))
   ,?assertEqual(false, check("1.2.1",">1.2.3"))
   ,?assertEqual(false, check("1.1.3",">1.2.3"))
   ,?assertEqual(false, check("0.2.1",">1.2.3"))
    %  <1.2.3
   ,?assertEqual(true, check("1.2.2","<1.2.3"))
   ,?assertEqual(true, check("1.1.3","<1.2.3"))
   ,?assertEqual(true, check("0.2.3","<1.2.3"))
   ,?assertEqual(false, check("1.2.4","<1.2.3"))
   ,?assertEqual(false, check("1.3.2","<1.2.3"))
   ,?assertEqual(false, check("2.2.1","<1.2.3"))
    % >=1.2.3
   ,?assertEqual(true, check("1.2.3",">=1.2.3"))
   ,?assertEqual(true, check("1.2.4",">=1.2.3"))
   ,?assertEqual(true, check("1.4.3",">=1.2.3"))
   ,?assertEqual(true, check("2.3.1",">=1.2.3"))
   ,?assertEqual(false, check("1.2.1",">=1.2.3"))
   ,?assertEqual(false, check("1.1.3",">=1.2.3"))
   ,?assertEqual(false, check("0.2.1",">=1.2.3"))
    % <=1.2.3
   ,?assertEqual(true, check("1.2.3","<=1.2.3"))
   ,?assertEqual(true, check("1.2.2","<=1.2.3"))
   ,?assertEqual(true, check("1.1.3","<=1.2.3"))
   ,?assertEqual(true, check("0.2.3","<=1.2.3"))
   ,?assertEqual(false, check("1.2.4","<=1.2.3"))
   ,?assertEqual(false, check("1.3.2","<=1.2.3"))
   ,?assertEqual(false, check("2.3.1","<=1.2.3"))
    %  ~1.2.3   is >=1.2.3 <1.3.0 
   ,?assertEqual(true, check("1.2.3","~1.2.3"))
   ,?assertEqual(true, check("1.2.4","~1.2.3"))
   ,?assertEqual(true, check("1.2.30","~1.2.3"))
   ,?assertEqual(false, check("1.3.0","~1.2.3"))
   ,?assertEqual(false, check("1.4.3","~1.2.3"))
   ,?assertEqual(false, check("2.2.3","~1.2.3"))
    %  ^1.2.3   is >=1.2.3 <2.0.0 
   ,?assertEqual(true, check("1.2.3","^1.2.3"))
   ,?assertEqual(true, check("1.2.4","^1.2.3"))
   ,?assertEqual(true, check("1.3.3","^1.2.3"))
   ,?assertEqual(true, check("1.100.3","^1.2.3"))
   ,?assertEqual(false, check("2.0.0","^1.2.3"))
   ,?assertEqual(false, check("2.2.3","^1.2.3"))
    %  ^0.2.3   is >=0.2.3 <0.3.0   (0.x.x is special)
   ,?assertEqual(true, check("0.2.3","^0.2.3"))
   ,?assertEqual(true, check("0.2.4","^0.2.3"))
   ,?assertEqual(true, check("0.2.50","^0.2.3"))
   ,?assertEqual(false, check("0.3.0","^0.2.3"))
   ,?assertEqual(false, check("0.3.1","^0.2.3"))
   ,?assertEqual(false, check("1.0.0","^0.2.3"))
   ,?assertEqual(false, check("1.1.0","^0.2.3"))
   ,?assertEqual(false, check("1.1.1","^0.2.3"))
    %  ^0.0.1   is =0.0.1           (0.0.x is special)
   ,?assertEqual(true, check("0.0.1","^0.0.1"))
   ,?assertEqual(false, check("0.0.2","^0.0.1"))
   ,?assertEqual(false, check("0.0.50","^0.0.1"))
   ,?assertEqual(false, check("0.1.1","^0.0.1"))
   ,?assertEqual(false, check("1.0.1","^0.0.1"))
   ,?assertEqual(false, check("1.1.1","^0.0.1"))
    %   0.x.x   is for “initial development”
   ,?assertEqual(true, check("0.1.0","0.x.x"))
   ,?assertEqual(true, check("0.1.2","0.x.x"))
   ,?assertEqual(true, check("0.2.1","0.x.x"))
   ,?assertEqual(false, check("1.0.0","0.x.x"))
   ,?assertEqual(false, check("1.1.0","0.x.x"))
   ,?assertEqual(false, check("2.0.2","0.x.x"))
    %   1.x.x   means public API is defined
   ,?assertEqual(true, check("1.0.0","1.x.x"))
   ,?assertEqual(true, check("1.0.2","1.x.x"))
   ,?assertEqual(true, check("1.3.2","1.x.x"))
    %  ^1.2     is >=1.2.0 <2.0.0   (like ^1.2.0)
   ,?assertEqual(true, check("1.2","^1.2"))
   ,?assertEqual(true, check("1.2.0","^1.2"))
   ,?assertEqual(true, check("1.2.5","^1.2"))
   ,?assertEqual(true, check("1.4.5","^1.2"))
   ,?assertEqual(false, check("2.0.0","^1.2"))
   ,?assertEqual(false, check("2.1.0","^1.2"))
   ,?assertEqual(false, check("2.2.3","^1.2"))
    %  ~1.2     is >=1.2.0 <1.3.0   (like ~1.2.0)
   ,?assertEqual(true, check("1.2","~1.2"))
   ,?assertEqual(true, check("1.2.0","~1.2"))
   ,?assertEqual(true, check("1.2.3","~1.2"))
   ,?assertEqual(true, check("1.2.50","~1.2"))
   ,?assertEqual(false, check("1.3.0","~1.2"))
   ,?assertEqual(false, check("1.3.5","~1.2"))
   ,?assertEqual(false, check("2.0.0","~1.2"))
   ,?assertEqual(false, check("2.1.0","~1.2"))
   ,?assertEqual(false, check("2.0.2","~1.2"))
   ,?assertEqual(false, check("2.2.2","~1.2"))
    %  ^1       is >=1.0.0 <2.0.0
   ,?assertEqual(true, check("1","^1"))
   ,?assertEqual(true, check("1.0","^1"))
   ,?assertEqual(true, check("1.0.0","^1"))
   ,?assertEqual(false, check("0","^1"))
   ,?assertEqual(false, check("0.1.0","^1"))
   ,?assertEqual(false, check("0.0.1","^1"))
   ,?assertEqual(false, check("2","^1"))
   ,?assertEqual(false, check("2.0.0","^1"))
   ,?assertEqual(false, check("2.0.1","^1"))
   ,?assertEqual(false, check("2.2.1","^1"))
    %  ~1       is >=1.0.0 <2.0.0 
   ,?assertEqual(true, check("1","~1"))
   ,?assertEqual(true, check("1.0","~1"))
   ,?assertEqual(true, check("1.0.0","~1"))
   ,?assertEqual(true, check("1.1","~1"))
   ,?assertEqual(true, check("1.1.1","~1"))
   ,?assertEqual(true, check("1.2.3","~1"))
   ,?assertEqual(false, check("2.0.0","~1"))
   ,?assertEqual(false, check("2.1.0","~1"))
   ,?assertEqual(false, check("2.2.3","~1"))
   ,?assertEqual(false, check("0.0.3","~1"))
   ,?assertEqual(false, check("0.1.0","~1"))
   ,?assertEqual(false, check("0.2.3","~1"))
    %   1.x     is >=1.0.0 <2.0.0 
   ,?assertEqual(true, check("1.0.0","1.x"))
   ,?assertEqual(true, check("1.0.2","1.x"))
   ,?assertEqual(true, check("1.1.0","1.x"))
   ,?assertEqual(true, check("1.1.2","1.x"))
   ,?assertEqual(true, check("1.30.2","1.x"))
   ,?assertEqual(false, check("2.0.0","1.x"))
   ,?assertEqual(false, check("2.1.0","1.x"))
   ,?assertEqual(false, check("2.2.1","1.x"))
    %   1.*     is >=1.0.0 <2.0.0 
   ,?assertEqual(true, check("1.0.0","1.*"))
   ,?assertEqual(true, check("1.0.2","1.*"))
   ,?assertEqual(true, check("1.1.0","1.*"))
   ,?assertEqual(true, check("1.1.2","1.*"))
   ,?assertEqual(true, check("1.30.2","1.*"))
   ,?assertEqual(false, check("2.0.0","1.*"))
   ,?assertEqual(false, check("2.1.0","1.*"))
   ,?assertEqual(false, check("2.2.1","1.*"))
    %   1       is >=1.0.0 <2.0.0 
   ,?assertEqual(true, check("1.0.0","1"))
   ,?assertEqual(true, check("1.0.2","1"))
   ,?assertEqual(true, check("1.1.0","1"))
   ,?assertEqual(true, check("1.1.2","1"))
   ,?assertEqual(true, check("1.30.2","1"))
   ,?assertEqual(false, check("2.0.0","1"))
   ,?assertEqual(false, check("2.1.0","1"))
   ,?assertEqual(false, check("2.2.1","1"))
    %   *       any version 
   ,?assertEqual(true, check("1","*"))
   ,?assertEqual(true, check("1.1","*"))
   ,?assertEqual(true, check("1.2.3","*"))
   ,?assertEqual(true, check("100.2.3","*"))
    %   x       any version
   ,?assertEqual(true, check("1","x"))
   ,?assertEqual(true, check("1.1","x"))
   ,?assertEqual(true, check("1.2.3","x"))
   ,?assertEqual(true, check("100.2.3","x"))
   ,ok.

check_comb_test() ->
    ?assertEqual(true,  check("1.4","~1.2 || 1.4"))
   ,?assertEqual(true, check("1.4.1","~1.2 || 1.4"))
   ,?assertEqual(true,  check("1.2.7","~1.2 || 1.4"))
   ,?assertEqual(true,  check("1.4.0","~1.2 || 1.4"))
   ,?assertEqual(true, check("1.4.2","~1.2 || 1.4"))
   ,?assertEqual(false, check("1.5.0","~1.2 || 1.4"))
   ,?assertEqual(true,  check("1.4.3","~1.2 || ~1.4"))
   ,?assertEqual(false, check("1.5","~1.2 || ~1.4"))
   ,?assertEqual(false, check("1.5.1","~1.2 || ~1.4"))
   ,?assertEqual(false, check("1.1",">1.2  <1.4"))
   ,?assertEqual(true,  check("1.3.3",">1.2  <1.4"))
   ,?assertEqual(false, check("1.4.0",">1.2  <1.4"))
   ,?assertEqual(true,  check("1.4.0",">1.2  <=1.4"))
   ,?assertEqual(false, check("1.4.3",">1.2  <=1.4"))
   ,?assertEqual(true,  check("R16B03-1",">R16B  <=17.1"))
   ,?assertEqual(true,  check("R16B03-1",">=R16B03  <=17.1"))
   ,?assertEqual(false, check("1.4.1",">=1.0 <=1.3 || >=1.5 <=1.8"))
   ,?assertEqual(true, check("1.2.1",">=1.0 <=1.3 || >=1.5 <=1.8"))
   ,?assertEqual(true, check("1.6",">=1.0 <=1.3 || >=1.5 <=1.8"))
   ,?assertEqual(true, check("2.4.1",">=1.0 <=1.3 || >=1.5 <=1.8 || >2.3"))
   ,ok.

misc_test() ->    
     ?assertEqual(17,  major("17.8.9-rc1+build001"))
    ,?assertEqual( 8,  minor("17.8.9-rc1+build001"))
    ,?assertEqual( 9,  patch("17.8.9-rc1+build001"))
    ,?assertEqual("rc1+build001",  suffix("17.8.9-rc1+build001"))
    ,?assertEqual("rc1",           prerelease("17.8.9-rc1+build001"))
    ,?assertEqual("build001",      build("17.8.9-rc1+build001"))
    ,ok.

-endif.