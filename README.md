# samovar

## Overview

`samovar` is an Erlang library implementing [SEMVER](https://semver.org/) standard.

## API


### Check a version vs a semver range

`samovar:check(Version :: list(), Range :: list()) -> boolean() || {error, Reason :: atom()}.`

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

## Syntax for old Erlang/OTP release

Old Erlang/OTP releases version will be internally mapped to syntaxically correct SEMVER version, on the fly,
both for version and range.

For instance `R16B` will be transcoded to `16.2.0` , or `R16B03-1` transcoded to `16.2.3-1`

This imply that `true = samovar:check("R16B03-1", ">R16B <21.2")` will be correct even if versions syntax is invalid from SEMVER standard perspective.