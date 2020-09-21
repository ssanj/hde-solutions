# Problem Definition 3

You are given a checkout directory, a folder path and a config directory. Together the checkout directory + folder path is the project directory. you need to look for an executable script (`script.sh`) in the following order:

- Look for the script in the config folder under the supplied folder path. if found execute it and return the result

Eg:
```
projectDir = checkoutDir/some/folder/path
config/some/folder/path/script.sh -- script found, so execute it
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
config/haskell/script.sh -- if this file exists then execute it
```

- If a matching executable is not found in the config directory for the language, execute a default handler in the root of the config directory that outputs the following the message to stdout: "no setup needed"

Eg:
```
config/script.sh -- execute this default file
```

The solution needs to be easily updated to include more languages (and build files).

Every location searched for should be logged to stdout (possibily at the end of the program):

Example execution log:
```
searching projectDir/some/folder/path
Didn't find matching checkoutDir/some/folder/path
searching projectDir/some/folder/path for scala build file (build.sbt)
searching projectDir/some/folder/path for ruby build file (Gemfile)
searching projectDir/some/folder/path for haskell build files (*.cabal, stack.yaml)
found haskell build file
looking for configDir/haskell/script.sh
haskell script file not found
executing default script at: configDir/script.sh
```

If any of the script executions fail, write the following to stdout: "script execution failed for *script path* because of *some error message*"

## Testing Criteria

You need to be able to test the following conditions

- If the project directory has a corresponding folder path in the config directory with an executable script, it gets executed.
- if the project directory has a corresponding folder path in the config directory without an executable script, fall back to language-based execution.
- The project directory has no matching languages, and the default script should be executed
- If the project directory matches the Scala language and has a matching script under configDir/scala folder. Verify that the script for scala is executed.
- If the project directory matches the Ruby language and doesn't have a matching script under config/ruby folder, this should search for other languages or return run the default script if none are found
- The execution log tracks the path taken to find the executable script
- When a script execution fails, the default error handler writes a message to stdout.