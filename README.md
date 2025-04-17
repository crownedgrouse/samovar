# samovar ![Build status](https://github.com/crownedgrouse/samovar/actions/workflows/erlang.yml/badge.svg) #

## Overview

`samovar` is an Erlang library implementing [SEMVER](https://semver.org/) standard.

Implementation is Erlang/OTP friendly by allowing use of old release version syntax.

Available on [hex.pm](https://hex.pm/packages/samovar) .

## Supported syntaxes

SEMVER by itself doesn't describe syntax of versions comparisons. 
`samovar` use : 
- [NPM](https://github.com/npm/node-semver#versions) standard, by default and target syntax.
- [Elixir](https://hexdocs.pm/elixir/Version.html#module-requirements) standard, since version 1.1. 
  - See [Elixir-dialect](https://github.com/crownedgrouse/samovar/wiki/Elixir-dialect) Wiki page for more details.

### Simple range
```
  1.2.3
 =1.2.3
 >1.2.3
 <1.2.3
>=1.2.3
 ~1.2.3   is >=1.2.3 <1.3.0 	 
 ^1.2.3   is >=1.2.3 <2.0.0 	 
 ^0.2.3   is >=0.2.3 <0.3.0   (0.x.x is special)
 ^0.0.1   is =0.0.1           (0.0.x is special)
  0.x.x   is for “initial development”
  1.x.x   means public API is defined
 ^1.2     is >=1.2.0 <2.0.0 	(like ^1.2.0)
 ~1.2     is >=1.2.0 <1.3.0 	(like ~1.2.0)
 ^1       is >=1.0.0 <2.0.0 	 
 ~1       is >=1.0.0 <2.0.0 	 
  1.x     is >=1.0.0 <2.0.0 	 
  1.*     is >=1.0.0 <2.0.0 	 
  1       is >=1.0.0 <2.0.0 	 
  *       any version 	 
  x       any version
```

### Hyphenated range
```
1.2.3 - 2.3.0   is >=1.2.3 <=2.3.0
                Partial right
                When the right is partial, missing pieces are assumed to be x (2.3 = 2.3.x).
1.2.3 - 2.3     is >=1.2.3 <2.4.0
1.2.3 - 2       is >=1.2.3 <3.0.0
                Partial left
                When the left is partial, missing pieces are assumed to be 0 (eg, 1.2.0).
1.2 - 2.3.0 	is 1.2.0 - 2.3.0
```

### Combining range
```
>=0.14 <16        And (space-separated)
0.14.x || 15.x.x  Or (pipe-separated)
```

## Syntax for old Erlang/OTP release

Old Erlang/OTP releases version will be internally mapped to syntaxically correct SEMVER version, on the fly,
both for version and range.

For instance `R16B` will be transcoded to `16.2.0` , or `R16B03-1` transcoded to `16.2.3-1`

This imply that `true = samovar:check("R16B03-1", ">R16B <21.2")` will be correct even if versions syntax is invalid from SEMVER standard perspective.

## API

### Check a version vs a semver range

```erlang
samovar:check(Version :: string(), Range :: string()) -> boolean() | {error, Reason :: atom()}.
```

```erlang

1> samovar:check("R16B03-1", ">R16B <21.2").
true
2> samovar:check("1.0.3", "1.0 - 1.1").
true
3> samovar:check("1.0.3", "~1.0").
true
4> samovar:check("1.1.2", "~1.0").
false
5> samovar:check("1.3", "<=1.2 || >1.4").
false
6> samovar:check("foo", "~1.2").
{error, invalid_version}
7> samovar:check("1.2", "foobar").
{error, invalid_range}

```

### Semversionize an Erlang/OTP release

```erlang
samovar:versionize(OTPVersion :: string()) -> SemverVersion :: string().
``` 

This function is internally used by `samovar` for old Erlang/OTP release version transcodification.
This function is however exported for users.

Note : this function is returning the input if no old Erlang/OTP release version is detected. Do not use this function to check correct semver syntax !

```erlang
1> samovar:versionize("17.5").
"17.5"
2> samovar:versionize("R16B02").
"16.2.2"
3> samovar:versionize("foo").  
"foo"

```

### Parse a semver version and get a proplist

```erlang
samovar:proplist(Version :: string()) ->  {ok, Proplist :: list()} | {error, Reason :: atom()}.
``` 

```erlang
1> samovar:proplist("14.5.8-rc1").
{ok,[{comp,[]},
     {major,"14"},
     {minor,"5"},
     {patch,"8"},
     {suffix,"rc1"},
     {pre,"rc1"},
     {build,[]}]}
```

Note : as `samovar` is intend to be usable on any Erlang/OTP release, map is not proposed as output format.
However proplist can easily be converted to map with : 
```erlang
1> {ok, P} = samovar:proplist("14.5.8-rc1"),
2> M = maps:from_list(P).   
#{build => [],comp => [],major => "14",minor => "5",
  patch => "8",pre => "rc1",suffix => "rc1"}
```

### Parse a semver version and get a record

```erlang
samovar:parse(Version :: string()) ->  {ok, Record :: tuple()} | {error, Reason :: atom()}.
``` 

This function allow to get a record with string version splitted into Major, Minor, Patch and Suffix elements.

Elements are still strings. See next functions for integer format.

```erlang
1> rr("include/samovar.hrl").
[version]
2> {ok, V} = samovar:parse("14.5.8-rc1").
{ok,#version{comp = [],major = "14",minor = "5",patch = "8",
             suffix = "rc1",pre = "rc1",build = []}}
3>  V#version.major . 
"14"
4> {ok, X} = samovar:parse("~14.5").
{ok,#version{comp = "~",major = "14",minor = "5",patch = [],
             suffix = [],pre = [],build = []}}
```

### Get individual elements of version string

```erlang
samovar:major(Version :: string()) -> Major :: integer().

samovar:minor(Version :: string()) -> Minor :: integer().

samovar:patch(Version :: string()) -> Patch :: integer().

samovar:suffix(Version :: string()) -> Suffix :: string().

samovar:prerelease(Version :: string()) -> PreRelease :: string().

samovar:build(Version :: string()) -> Build :: string().
```

```erlang
1> samovar:major("17.8.9-rc1+build001").
17
2> samovar:minor("17.8.9-rc1+build001").
8
3> samovar:patch("17.8.9-rc1+build001").
9
4> samovar:suffix("17.8.9-rc1+build001").
"rc1+build001"
5> samovar:prerelease("17.8.9-rc1+build001").
"rc1"
6> samovar:build("17.8.9-rc1+build001").
"build001"

```



