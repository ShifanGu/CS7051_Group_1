# repo-complexity-analyzer

## Introduction
In this project, repo-complexity-analyzer is designed to fetch the repositories all together and analysing them and filtering the Haskell files from it for the calculation of complexity. We will use Argon Haskell library for the calculation of complexity. It is a quantitative measure of the number of linearly independent paths through the code. The intended usage is through Argon's executable, which accepts a list of files or directories to analyse. The data can be optionally exported to JSON. The files which we get after the analysis done, we are trying to calculate the complexity with different methods for project level and file level.

## Useage

```sh
$ git clone https://github.com/ShifanGu/repo-complexity-analyzer.git
```
Then go the the project root
```sh
$ stack setup
$ stack build
```
Then run

```sh
$ stack exec repo-complexity-analyer
```
open the browser
```sh
http://localhost:8080/methodComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the method level complexity of the project

```json
[{"blocks":[{"complexity":2,"name":"insert","lineno":69,"col":1},{"complexity":2,"name":"inorder","lineno":89,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":106,"col":1}],"path":"./haskelldocumentation/exercises/BinTree/BinTree.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":12,"col":1},{"complexity":1,"name":"propSorted","lineno":29,"col":17}],"path":"./haskelldocumentation/exercises/BinTree/BinTreeSpec.hs","type":"result"},

...

{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"twoOrThree","lineno":14,"col":13},{"complexity":1,"name":"expected","lineno":15,"col":13},{"complexity":1,"name":"evensExceptTwo","lineno":20,"col":13}],"path":"./haskelldocumentation/exercises/HigherOrderFunctions/HigherOrderFunctionsSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"main","lineno":2,"col":1}],"path":"./haskelldocumentation/src/soh-upload/Setup.hs","type":"result"}]
```


```sh
http://localhost:8080/fileComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the file level complexity of the project

```js
[["./haskelldocumentation/exercises/BinTree/BinTree.hs",1.6666666666666667],["./haskelldocumentation/exercises/BinTree/BinTreeSpec.hs",1],["./haskelldocumentation/exercises/Functions/Functions.hs",1],["./haskelldocumentation/exercises/Functions/FunctionsSpec.hs",4]

...

,["./haskelldocumentation/exercises/UpdateRecords/UpdateRecordsSpec.hs",1],["./haskelldocumentation/exercises/Vigenere/Vigenere.hs",1],["./haskelldocumentation/exercises/Vigenere/VigenereSpec.hs",1],["./haskelldocumentation/src/soh-upload/Main.hs",1.3113207547169812],["./haskelldocumentation/src/soh-upload/Preprocess.hs",1.625],["./haskelldocumentation/src/soh-upload/Setup.hs",1]]
```

```sh
http://localhost:8080/methodComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the project level complexity of the project

```js
["haskelldocumentation",38.50774932614555]
```
##Batch Process
This project also provided a batch process script named <code>batch-analyze.sh</code> which contains 45 haskell repositories.

to use this script 

execute the project
```sh
$ stack exec repo-complexity-analyer
```
then open another terminal and simpley run 
```sh
$ sh batch-analyze.sh
```
JSON result of this process is provide in the <code>repo-complexity.json</code> file

```js
{{"99haskell":8.119871794871795},{"consul-haskell":3.739583333333333},{"dash-haskell":29.89694749694749},{"detexify-hs-backend":14.432945826551965},{"ethereum-haskell":22.008439387597605},{"flappy-haskell":8.918552036199095},{"graphql-haskell":11.298744650602234},{"haskell":45.08470418470418},{"Haskell-1":159.108547008547},{"haskell-buildpack-demo":2},{"haskell-couchdb":8.491946750960034},{"haskell-course-ru":8.312907268170427},{"haskell-cypher":4.501831501831502},{"haskell-docs":13.198076923076924},{"haskell-jobqueue":48.942948717948724},{"haskell-kubernetes":134.66666666666666},{"haskell-memcached":5.423992673992673},{"haskell-ml":50.472727272727276},{"haskell-must-watch":0},{"haskell-names":77.53644457394458},{"haskell-opaleye":105.19813922322945},{"haskell-platform":46.009651864900356},{"haskell-sdl2-examples":46.27841142751357},{"haskell-snake":1.3529411764705883},{"haskell-telegram-api":11.116287878787878},{"haskell-tor":39.17943607810628},{"haskell-wayland":9.459001782531194},{"haskell_hadoop":6.545454545454545},{"haskellbook":114.90997732990448},{"haskelldocumentation":38.50774932614555},{"haskellers":31.622073253543846},{"HaskellKoans":10.066666666666666},{"hell":8.352941176470589},{"ldap-haskell":4.153846153846153},{"learn-you-a-haskell-exercises":11},{"libmpd-haskell":67.89160852454584},{"msgpack-haskell":48.532836843006336},{"nanomsg-haskell":7.148717948717948},{"NetASM-haskell":13.966355140186915},{"riak-haskell-client":77.28651071151069},{"sparkle":20.33859649122807},{"stylish-haskell":33.26644289533995},{"summer-2015-haskell-class":19.11182670696737},{"tokyocabinet-haskell":46.28102411781346},{"zeromq-haskell":15.058386855886518}}
```



##Author
This project is created and maintained by [Shifan Gu](https://github.com/ShifanGu/), [Andrew Comey](https://github.com/acomey), [Kanika Shreya](https://github.com/shreyakanika15) and Aideen Darker. 
