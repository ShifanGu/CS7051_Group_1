# repo-complexity-analyzer

## Introduction
In this project, repo-complexity-analyzer is designed to fetch the repositories all together and analysing them and filtering the Haskell files from it for the calculation of complexity. We will use Argon Haskell library for the calculation of complexity. It is a quantitative measure of the number of linearly independent paths through the code. The intended usage is through Argon's executable, which accepts a list of files or directories to analyse. The data can be optionally exported to JSON. The files which we get after analysing we are trying to calculate the complexity with method, file and project level.

## Useage

```sh
git clone https://github.com/ShifanGu/repo-complexity-analyzer.git
```
Then go the the project root
```sh
stack setup
stack build
```
Then run

```sh
stack exec repo-complexity-analyer
```
open the browser
```sh
localhost:8080/methodComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```