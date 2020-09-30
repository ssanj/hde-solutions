# Problem Definition 1

Given a project directory you need to search the project directory for build file that matches a particular language. The build file may not be found.

Assume the following languages and build files:
  - Scala   -> build.sbt
  - Ruby    -> Gemfile
  - Haskell -> Any .cabal file or a stack.yaml file

Write a function that searches through a project directory and searches for the above language build files and returns the first match found, in the form of a Language.

The solution needs to be easily updated to include more languages (and build files).

Create any data structures necessary. Use any libraries you like.

## Testing Criteria

You need to be able to test the following conditions:

### case: The project directory has no matching files

### case: The project directory has a matching language, return language found

### case: The project directory has multiple matching languages, return the first matching language found
