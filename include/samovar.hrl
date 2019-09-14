%% semver record
-record(version, { comp = "=" :: string() % Compare string
                 , major= ""  :: string()
                 , minor= ""  :: string()
                 , patch= ""  :: string()
                 , suffix=""  :: string() % Suffix (prerelease or build or both)
                 , pre=""     :: string() % Prerelease
                 , build=""   :: string() % Build metadata
                 }).
