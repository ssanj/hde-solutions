# Problem Definition 3

You are given a checkout directory, a folder path and a config directory. Together the checkout directory + folder path form the project directory. You need to look for an executable script (`script.sh`) in the following order:

- Look for the script in the config folder under the supplied folder path. if found execute it and return the result

Eg:
```
projectDir = checkoutDir/some/folder/path
configDir/some/folder/path/script.sh -- script found, so execute it
```

- If that fails then you need to search the project for build file that matches a particular language. The build file may not be found.

Assume the following languages and build files:
  - Scala   -> build.sbt
  - Ruby    -> Gemfile
  - Haskell -> Any .cabal file or a stack.yaml file

If the build file is found you need to see if an executable script exists in the config directory for the language. If one is found you need to execute it.

Eg:
```
projectDir/some/folder/path/stack.yaml -- Haskell build file found
configDir/haskell/script.sh -- if this file exists then execute it
```

- If a matching executable is not found in the config directory for the language, execute a default handler in the root of the config directory that outputs the following the message to stdout: "no setup needed"

Eg:
```
configDir/script.sh -- execute this default file
```

The solution needs to be easily updated to include more languages (and build files).

Every location searched for should be logged to stdout (possibly at the end of the program):

Example execution log:
```
searching for project script in configDir/some/folder/path
searching for scala language build file (build.sbt) in checkoutDir/some/folder/path
searching for ruby language build file (Gemfile) in checkoutDir/some/folder/path
searching for haskell language build file (*.cabal) in checkoutDir/some/folder/path
searching for haskell language build file (stack.yaml) in checkoutDir/some/folder/path
found haskell build file (stack.yaml)
looking for haskell script in configDir/haskell/script.sh
executing default script at: configDir/script.sh
```

If any of the script executions fail, write the following to stdout: "script execution failed for `<script path>` because of `<error reason>`", where `<script>` path is the path to the script that failed and `<error reason>` is the reason it failed.

## Testing Criteria

You need to be able to test the following conditions:

### case: Has a project script, runs project script

If the project directory has a corresponding folder path in the config directory with an executable script, it gets executed and the output result is as expected. Ensure no other scripts are executed and languages are not searched for.

### case: No project script, has language script, runs language script

Look for a matching language in the project folder and execute the script in the config/language folder if found. Ensure no other scripts are executed.

### case: No project script, no language script, runs default script

If either the language or language script is not found then the default script should be executed and the output verified.

### case: If any script execution fails ensure the default handler is invoked

### case: Given a project that does not have a project script or language script ensure the default script is executed and the log follows all the options tried.

Verify the execution log tracks the path taken to find the default script
