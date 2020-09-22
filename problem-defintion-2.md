# Problem Definition 2

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

Don't handle an file or execution errors at this point. Assume the default script exists and all scripts are executable.

## Testing Criteria

You need to be able to test the following conditions

- If the project directory has a corresponding folder path in the config directory with an executable script, it gets executed.
- If the project directory has a corresponding folder path in the config directory without an executable script, fall back to language-based execution.
- If the project directory has no matching languages, and the default script should be executed.
- If the project directory matches the Scala language and has a matching script under configDir/scala folder. Verify that the script for scala is executed.
- If the project directory matches the Ruby language and doesn't have a matching script under config/ruby folder. Continue to search for other languages or run the default script if none are found
