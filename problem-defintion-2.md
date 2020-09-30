# Problem Definition 2

You are given a checkout directory, a folder path and a config directory. Together the checkout directory + folder path form the project directory. You need to look for an executable script (`script.sh`) in the following order:

- Look for the script in the config folder under the supplied folder path. If found execute it and return the result (lines of output text).

Eg:
```
projectDir = checkoutDir/some/folder/path
configDir/some/folder/path/script.sh -- script found, so execute it
```

- If a script is not found you need to search the project for build file that matches a particular language. The build file may not be found.

Assume the following languages and build files:
  - Scala   -> build.sbt
  - Ruby    -> Gemfile
  - Haskell -> Any .cabal file or a stack.yaml file

If the build file is found you need to see if an executable script exists in the config directory for the language. The language script is found under `config/language` where the language name is lowercased. If one is found you need to execute it and return the result (lines of output text)

Eg:
```
projectDir/some/folder/path/stack.yaml -- Haskell build file found
configDir/haskell/script.sh -- if this file exists then execute it
```

- If a matching executable is not found in the config directory for the language, execute a default handler in the root of the config directory that outputs the following line of text : "no setup needed"

Eg:
```
configDir/script.sh -- execute this default file
```

The solution needs to be easily updated to include more languages (and build files).

Don't handle an file or execution errors at this point. Assume the default script exists and all scripts are executable.

The output of each script needs to be verified (if run)

Create any data structures necessary. Use any libraries you like.

## Testing Criteria

You need to be able to test the following conditions:

### case: Has a project script, runs project script

If the project directory has a corresponding folder path in the config directory with an executable script, it gets executed and the output result is as expected. Ensure no other scripts are executed and languages are not searched for.

### case: No project script, has language script, runs language script

Look for a matching language in the project folder and execute the script in the config/language folder if found. Ensure no other scripts are executed.

### case: No project script, no language script, runs default script

If either the language or language script is not found then the default script should be executed and the output verified.
