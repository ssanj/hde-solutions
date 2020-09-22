# Problem Definition 1


Given a project directory you need to search the project directory for build file that matches a particular language. The build file may not be found.

Assume the following languages and build files:
  - Scala   -> build.sbt
  - Ruby    -> Gemfile
  - Haskell -> Any .cabal file or a stack.yaml file

Write a function that searches through a project directory and searches for the above language build files and returns the first match found, in the form of a Language.

The solution needs to be easily updated to include more languages (and build files).

## Testing Criteria

You need to be able to test the following conditions

- The project directory has no matching languages
- The project directory matches the Scala language
- The project directory has many matching languages but returns the first match
- The project directory matches a Haskell project by ".cabal" file
- The project directory matches a Haskell project by "stack.yaml" file
