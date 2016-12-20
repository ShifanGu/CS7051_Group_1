# repo-complexity-analyzer

## Introduction
In this project, repo-complexity-analyzer is designed to fetch the repositories all together and analysing them and filtering the Haskell files from it for the calculation of complexity. We will use Argon Haskell library for the calculation of complexity. It is a quantitative measure of the number of linearly independent paths through the code. The intended usage is through Argon's executable, which accepts a list of files or directories to analyse. The data can be optionally exported to JSON. The files which we get after analysing we are trying to calculate the complexity with method, file and project level.

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
localhost:8080/methodComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the method level complexity of the project

```json
[{"blocks":[{"complexity":2,"name":"insert","lineno":69,"col":1},{"complexity":2,"name":"inorder","lineno":89,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":106,"col":1}],"path":"./haskelldocumentation/exercises/BinTree/BinTree.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":12,"col":1},{"complexity":1,"name":"propSorted","lineno":29,"col":17}],"path":"./haskelldocumentation/exercises/BinTree/BinTreeSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"welcome","lineno":9,"col":1},{"complexity":1,"name":"welcomeSirOrMadam","lineno":19,"col":1},{"complexity":1,"name":"printWelcomeMessage2","lineno":26,"col":1},{"complexity":1,"name":"multiply","lineno":44,"col":1},{"complexity":1,"name":"multiply10by20","lineno":46,"col":1},{"complexity":1,"name":"plus","lineno":54,"col":1},{"complexity":1,"name":"sum3","lineno":63,"col":1},{"complexity":1,"name":"isDollar","lineno":72,"col":1},{"complexity":1,"name":"xor","lineno":84,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":86,"col":1}],"path":"./haskelldocumentation/exercises/Functions/Functions.hs","type":"result"},{"blocks":[{"complexity":4,"name":"spec","lineno":8,"col":1}],"path":"./haskelldocumentation/exercises/Functions/FunctionsSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"evens","lineno":20,"col":1},{"complexity":1,"name":"positives","lineno":24,"col":1},{"complexity":1,"name":"divBy3","lineno":28,"col":1},{"complexity":1,"name":"emptySet","lineno":32,"col":1},{"complexity":2,"name":"intersect","lineno":42,"col":1},{"complexity":1,"name":"memberOfA","lineno":42,"col":27},{"complexity":1,"name":"memberOfB","lineno":43,"col":27},{"complexity":1,"name":"mySet1","lineno":47,"col":1},{"complexity":1,"name":"singleton","lineno":55,"col":1},{"complexity":1,"name":"union","lineno":63,"col":1},{"complexity":1,"name":"difference","lineno":73,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":75,"col":1}],"path":"./haskelldocumentation/exercises/HigherOrderFunctions/HigherOrderFunctions.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"twoOrThree","lineno":14,"col":13},{"complexity":1,"name":"expected","lineno":15,"col":13},{"complexity":1,"name":"evensExceptTwo","lineno":20,"col":13}],"path":"./haskelldocumentation/exercises/HigherOrderFunctions/HigherOrderFunctionsSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"parseLine","lineno":67,"col":1},{"complexity":1,"name":"validLogLines","lineno":77,"col":1},{"complexity":1,"name":"parse","lineno":88,"col":1},{"complexity":1,"name":"importantOnly","lineno":98,"col":1},{"complexity":1,"name":"sorted","lineno":109,"col":1}],"path":"./haskelldocumentation/exercises/LogParser/LogParser.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":7,"col":1},{"complexity":1,"name":"input1","lineno":9,"col":13},{"complexity":1,"name":"input2","lineno":15,"col":13},{"complexity":1,"name":"garbage","lineno":21,"col":13},{"complexity":1,"name":"input","lineno":35,"col":17}],"path":"./haskelldocumentation/exercises/LogParser/LogParserSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"safeDiv","lineno":27,"col":1},{"complexity":2,"name":"safeHead","lineno":35,"col":1},{"complexity":1,"name":"f","lineno":57,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":84,"col":1}],"path":"./haskelldocumentation/exercises/Maybe/Maybe.hs","type":"result"},{"blocks":[{"complexity":3,"name":"spec","lineno":12,"col":1},{"complexity":3,"name":"propDiv","lineno":16,"col":17},{"complexity":3,"name":"propHead","lineno":29,"col":17}],"path":"./haskelldocumentation/exercises/Maybe/MaybeSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"emptyList","lineno":7,"col":1},{"complexity":1,"name":"nonEmptyList","lineno":8,"col":1},{"complexity":1,"name":"firstElement","lineno":16,"col":1},{"complexity":1,"name":"restOfList","lineno":17,"col":1},{"complexity":1,"name":"thirdElement","lineno":18,"col":1},{"complexity":1,"name":"secondAndLastElement","lineno":19,"col":1},{"complexity":1,"name":"prettySyntax","lineno":25,"col":1},{"complexity":1,"name":"desugaredList","lineno":29,"col":1},{"complexity":1,"name":"secondElement","lineno":56,"col":1},{"complexity":1,"name":"drop3","lineno":65,"col":1},{"complexity":1,"name":"thirdAndLast","lineno":73,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":81,"col":1}],"path":"./haskelldocumentation/exercises/PatternMatching/PatternMatching.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"input","lineno":10,"col":13},{"complexity":1,"name":"expected","lineno":11,"col":13},{"complexity":1,"name":"input","lineno":16,"col":13},{"complexity":1,"name":"expected","lineno":17,"col":13},{"complexity":1,"name":"input","lineno":22,"col":13},{"complexity":1,"name":"expected","lineno":23,"col":13}],"path":"./haskelldocumentation/exercises/PatternMatching/PatternMatchingSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"problem1","lineno":17,"col":1},{"complexity":1,"name":"problem4","lineno":36,"col":1},{"complexity":1,"name":"problem6","lineno":53,"col":1},{"complexity":1,"name":"problem16","lineno":62,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":64,"col":1}],"path":"./haskelldocumentation/exercises/ProjectEuler/ProjectEuler.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1}],"path":"./haskelldocumentation/exercises/ProjectEuler/ProjectEulerSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"reverseProperty","lineno":9,"col":1},{"complexity":1,"name":"reverseProperty'","lineno":14,"col":1},{"complexity":1,"name":"encode","lineno":32,"col":1},{"complexity":1,"name":"decode","lineno":45,"col":1}],"path":"./haskelldocumentation/exercises/QuickCheck/QuickCheck.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1}],"path":"./haskelldocumentation/exercises/QuickCheck/QuickCheckSpec.hs","type":"result"},{"blocks":[{"complexity":2,"name":"downToZero","lineno":12,"col":1},{"complexity":2,"name":"returnLastElementPartial","lineno":24,"col":1},{"complexity":3,"name":"returnLastElement2","lineno":40,"col":1},{"complexity":1,"name":"secondToLast","lineno":55,"col":1},{"complexity":1,"name":"listLength","lineno":67,"col":1},{"complexity":1,"name":"listLengthRec","lineno":69,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":77,"col":1}],"path":"./haskelldocumentation/exercises/Recursion/Recursion.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"input","lineno":10,"col":13},{"complexity":1,"name":"expected","lineno":11,"col":13}],"path":"./haskelldocumentation/exercises/Recursion/RecursionSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"celebrities","lineno":7,"col":1},{"complexity":1,"name":"nameLengths","lineno":12,"col":1},{"complexity":1,"name":"reversedNames","lineno":15,"col":1},{"complexity":1,"name":"isSimon","lineno":18,"col":1},{"complexity":1,"name":"startsWithS","lineno":21,"col":1},{"complexity":1,"name":"startsWithT","lineno":32,"col":1},{"complexity":1,"name":"add1","lineno":44,"col":1},{"complexity":1,"name":"numsAsStrings","lineno":53,"col":1},{"complexity":1,"name":"greaterThan2","lineno":61,"col":1},{"complexity":1,"name":"filterNot","lineno":69,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":71,"col":1}],"path":"./haskelldocumentation/exercises/RecursionSchemes/RecursionSchemes.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1}],"path":"./haskelldocumentation/exercises/RecursionSchemes/RecursionSchemesSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"find","lineno":24,"col":1},{"complexity":1,"name":"main","lineno":26,"col":1}],"path":"./haskelldocumentation/exercises/Regex/Regex.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"needle","lineno":12,"col":13},{"complexity":1,"name":"haystack","lineno":13,"col":13},{"complexity":1,"name":"regexTest","lineno":26,"col":1}],"path":"./haskelldocumentation/exercises/Regex/RegexSpec.hs","type":"result"},{"blocks":[],"path":"./haskelldocumentation/exercises/Spec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"stringNumberToDigitList","lineno":9,"col":1},{"complexity":1,"name":"digitListToString","lineno":12,"col":1},{"complexity":1,"name":"calculationNotYetImplemented","lineno":15,"col":1},{"complexity":1,"name":"textSum","lineno":30,"col":1},{"complexity":1,"name":"textSub","lineno":37,"col":1},{"complexity":1,"name":"textMul","lineno":45,"col":1}],"path":"./haskelldocumentation/exercises/TextCalculator/TextCalculator.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":8,"col":1},{"complexity":1,"name":"positivePairs","lineno":42,"col":1}],"path":"./haskelldocumentation/exercises/TextCalculator/TextCalculatorSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"sweden","lineno":14,"col":1},{"complexity":1,"name":"sweetBrother","lineno":22,"col":1},{"complexity":1,"name":"sweeterBrother","lineno":32,"col":1},{"complexity":1,"name":"backupDB","lineno":56,"col":1},{"complexity":1,"name":"backupDBWeekly","lineno":67,"col":1}],"path":"./haskelldocumentation/exercises/UpdateRecords/UpdateRecords.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":10,"col":1},{"complexity":1,"name":"correctBackupDBWeekly","lineno":16,"col":1}],"path":"./haskelldocumentation/exercises/UpdateRecords/UpdateRecordsSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"alphabet","lineno":40,"col":1},{"complexity":1,"name":"char2int","lineno":43,"col":1},{"complexity":1,"name":"int2char","lineno":46,"col":1},{"complexity":1,"name":"encryptChar","lineno":49,"col":1},{"complexity":1,"name":"decryptChar","lineno":52,"col":1},{"complexity":1,"name":"encrypt","lineno":55,"col":1},{"complexity":1,"name":"decrypt","lineno":58,"col":1},{"complexity":1,"name":"_YOUR_CODE_HERE","lineno":61,"col":1}],"path":"./haskelldocumentation/exercises/Vigenere/Vigenere.hs","type":"result"},{"blocks":[{"complexity":1,"name":"spec","lineno":7,"col":1}],"path":"./haskelldocumentation/exercises/Vigenere/VigenereSpec.hs","type":"result"},{"blocks":[{"complexity":1,"name":"defaultHost","lineno":51,"col":1},{"complexity":1,"name":"configFileName","lineno":54,"col":1},{"complexity":1,"name":"htmlParseOne","lineno":92,"col":1},{"complexity":3,"name":"go","lineno":94,"col":3},{"complexity":1,"name":"pure","lineno":101,"col":3},{"complexity":1,"name":"<*>","lineno":109,"col":6},{"complexity":1,"name":"parseJSON","lineno":121,"col":3},{"complexity":1,"name":"updateMetadata","lineno":130,"col":1},{"complexity":4,"name":"metadataEq","lineno":133,"col":1},{"complexity":1,"name":"parseJSON","lineno":142,"col":3},{"complexity":1,"name":"userAuthorization","lineno":144,"col":9},{"complexity":1,"name":"userPrefix","lineno":146,"col":9},{"complexity":3,"name":"getContext","lineno":153,"col":1},{"complexity":1,"name":"parseWithAuth","lineno":164,"col":1},{"complexity":1,"name":"getListReq","lineno":174,"col":1},{"complexity":1,"name":"path","lineno":175,"col":7},{"complexity":1,"name":"getTutorialReq","lineno":179,"col":1},{"complexity":1,"name":"path","lineno":180,"col":7},{"complexity":1,"name":"postNewTutorialReq","lineno":188,"col":1},{"complexity":1,"name":"path","lineno":189,"col":7},{"complexity":1,"name":"postMetadataReq","lineno":194,"col":1},{"complexity":1,"name":"body","lineno":196,"col":7},{"complexity":1,"name":"postSaveReq","lineno":202,"col":1},{"complexity":1,"name":"path","lineno":203,"col":7},{"complexity":1,"name":"body","lineno":205,"col":7},{"complexity":1,"name":"postPublishReq","lineno":212,"col":1},{"complexity":1,"name":"path","lineno":213,"col":7},{"complexity":1,"name":"postDelReq","lineno":218,"col":1},{"complexity":1,"name":"path","lineno":219,"col":7},{"complexity":1,"name":"markdownRegex","lineno":225,"col":1},{"complexity":2,"name":"toFile","lineno":228,"col":1},{"complexity":1,"name":"fBS","lineno":235,"col":9},{"complexity":1,"name":"getFiles","lineno":238,"col":1},{"complexity":5,"name":"parseFrontmatter","lineno":242,"col":1},{"complexity":2,"name":"decodeEitherFst","lineno":243,"col":3},{"complexity":1,"name":"ts","lineno":246,"col":3},{"complexity":1,"name":"isYamlMarker","lineno":247,"col":3},{"complexity":4,"name":"yamlBSEither","lineno":248,"col":3},{"complexity":1,"name":"yamlBS","lineno":253,"col":13},{"complexity":1,"name":"mdBS","lineno":254,"col":13},{"complexity":2,"name":"metadataUpdatesFromBS","lineno":260,"col":1},{"complexity":1,"name":"readFileBS","lineno":265,"col":1},{"complexity":4,"name":"saveUpdate","lineno":268,"col":1},{"complexity":1,"name":"newMetadata","lineno":279,"col":7},{"complexity":1,"name":"createNew","lineno":289,"col":1},{"complexity":1,"name":"freshMetadata","lineno":293,"col":7},{"complexity":1,"name":"metadataUpdates","lineno":294,"col":7},{"complexity":1,"name":"freshSlug","lineno":295,"col":7},{"complexity":1,"name":"updatedMetadata","lineno":296,"col":7},{"complexity":1,"name":"newTutorial","lineno":302,"col":1},{"complexity":1,"name":"reqBodyFromMetadata","lineno":308,"col":1},{"complexity":1,"name":"testNewTutorial","lineno":317,"col":1},{"complexity":1,"name":"testGetMetadata","lineno":324,"col":1},{"complexity":1,"name":"slug","lineno":327,"col":7},{"complexity":1,"name":"testSetMetadata","lineno":332,"col":1},{"complexity":1,"name":"slug","lineno":335,"col":7},{"complexity":1,"name":"updatesToMetadata","lineno":336,"col":7},{"complexity":1,"name":"getMetadata","lineno":349,"col":1},{"complexity":2,"name":"setMetadata","lineno":357,"col":1},{"complexity":1,"name":"newMetadata","lineno":359,"col":7},{"complexity":1,"name":"htmlParseConcurrencyToken","lineno":374,"col":1},{"complexity":1,"name":"parsed","lineno":375,"col":3},{"complexity":1,"name":"cursor","lineno":376,"col":3},{"complexity":1,"name":"selector","lineno":377,"col":3},{"complexity":2,"name":"htmlParseMetadata","lineno":382,"col":1},{"complexity":1,"name":"parsed","lineno":383,"col":3},{"complexity":1,"name":"html","lineno":384,"col":3},{"complexity":1,"name":"formSelector","lineno":385,"col":3},{"complexity":1,"name":"mdCsrfToken","lineno":387,"col":3},{"complexity":1,"name":"mdTitle","lineno":388,"col":3},{"complexity":1,"name":"mdDescription","lineno":389,"col":3},{"complexity":1,"name":"mdSlug","lineno":390,"col":3},{"complexity":1,"name":"mdPackageSet","lineno":391,"col":3},{"complexity":1,"name":"csrfTokenSelector","lineno":393,"col":3},{"complexity":1,"name":"titleSelector","lineno":394,"col":3},{"complexity":1,"name":"descriptionSelector","lineno":395,"col":3},{"complexity":1,"name":"slugSelector","lineno":396,"col":3},{"complexity":1,"name":"packageSelector","lineno":397,"col":3},{"complexity":1,"name":"inputValOf","lineno":399,"col":3},{"complexity":1,"name":"textareaValOf","lineno":401,"col":3},{"complexity":1,"name":"selectValOf","lineno":403,"col":3},{"complexity":2,"name":"childContentOr","lineno":408,"col":3},{"complexity":2,"name":"htmlParseContent","lineno":415,"col":1},{"complexity":1,"name":"parsed","lineno":416,"col":3},{"complexity":1,"name":"cursor","lineno":417,"col":3},{"complexity":1,"name":"selector","lineno":418,"col":3},{"complexity":1,"name":"textareaValOf","lineno":422,"col":3},{"complexity":2,"name":"childContentOr","lineno":424,"col":3},{"complexity":1,"name":"saveConfigFromLBS","lineno":429,"col":1},{"complexity":2,"name":"getSaveConfig","lineno":442,"col":1},{"complexity":3,"name":"handler404","lineno":448,"col":5},{"complexity":1,"name":"publishTutorial","lineno":455,"col":1},{"complexity":2,"name":"uploadFile","lineno":462,"col":1},{"complexity":1,"name":"uploadAll","lineno":474,"col":1},{"complexity":1,"name":"deleteItem","lineno":480,"col":1},{"complexity":3,"name":"deleteAllExceptFor","lineno":488,"col":1},{"complexity":1,"name":"cursor","lineno":492,"col":9},{"complexity":1,"name":"links","lineno":493,"col":9},{"complexity":1,"name":"slugs","lineno":494,"col":9},{"complexity":1,"name":"prefix","lineno":500,"col":5},{"complexity":1,"name":"selector","lineno":502,"col":5},{"complexity":1,"name":"pred'","lineno":507,"col":9},{"complexity":2,"name":"hasClass","lineno":509,"col":5},{"complexity":2,"name":"checkAttribute","lineno":514,"col":5},{"complexity":1,"name":"parseSuffix","lineno":519,"col":5},{"complexity":1,"name":"main","lineno":525,"col":1}],"path":"./haskelldocumentation/src/soh-upload/Main.hs","type":"result"},{"blocks":[{"complexity":2,"name":"hrefTweakEvent","lineno":25,"col":1},{"complexity":1,"name":"hrefTweakAttrs","lineno":30,"col":1},{"complexity":2,"name":"tweakIfHref","lineno":31,"col":3},{"complexity":2,"name":"tweakContent","lineno":33,"col":3},{"complexity":2,"name":"hrefTweak","lineno":40,"col":1},{"complexity":1,"name":"maybeStripped","lineno":42,"col":3},{"complexity":1,"name":"isAbsolute","lineno":47,"col":1},{"complexity":6,"name":"activeHaskellTweak","lineno":53,"col":1},{"complexity":1,"name":"withActiveHaskell","lineno":68,"col":5},{"complexity":2,"name":"activeHaskellClass","lineno":69,"col":5},{"complexity":1,"name":"eventModifications","lineno":77,"col":1},{"complexity":1,"name":"preprocessMarkdown","lineno":81,"col":1},{"complexity":1,"name":"afterStream","lineno":90,"col":5},{"complexity":1,"name":"renderedMarkdown","lineno":91,"col":5},{"complexity":1,"name":"lmd","lineno":92,"col":5},{"complexity":1,"name":"asIO","lineno":95,"col":5}],"path":"./haskelldocumentation/src/soh-upload/Preprocess.hs","type":"result"},{"blocks":[{"complexity":1,"name":"main","lineno":2,"col":1}],"path":"./haskelldocumentation/src/soh-upload/Setup.hs","type":"result"}]
```


```sh
localhost:8080/fileComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the file level complexity of the project

```json
[["./haskelldocumentation/exercises/BinTree/BinTree.hs",1.6666666666666667],["./haskelldocumentation/exercises/BinTree/BinTreeSpec.hs",1],["./haskelldocumentation/exercises/Functions/Functions.hs",1],["./haskelldocumentation/exercises/Functions/FunctionsSpec.hs",4],["./haskelldocumentation/exercises/HigherOrderFunctions/HigherOrderFunctions.hs",1.0833333333333333],["./haskelldocumentation/exercises/HigherOrderFunctions/HigherOrderFunctionsSpec.hs",1],["./haskelldocumentation/exercises/LogParser/LogParser.hs",1],["./haskelldocumentation/exercises/LogParser/LogParserSpec.hs",1],["./haskelldocumentation/exercises/Maybe/Maybe.hs",1.25],["./haskelldocumentation/exercises/Maybe/MaybeSpec.hs",3],["./haskelldocumentation/exercises/PatternMatching/PatternMatching.hs",1],["./haskelldocumentation/exercises/PatternMatching/PatternMatchingSpec.hs",1],["./haskelldocumentation/exercises/ProjectEuler/ProjectEuler.hs",1],["./haskelldocumentation/exercises/ProjectEuler/ProjectEulerSpec.hs",1],["./haskelldocumentation/exercises/QuickCheck/QuickCheck.hs",1],["./haskelldocumentation/exercises/QuickCheck/QuickCheckSpec.hs",1],["./haskelldocumentation/exercises/Recursion/Recursion.hs",1.5714285714285714],["./haskelldocumentation/exercises/Recursion/RecursionSpec.hs",1],["./haskelldocumentation/exercises/RecursionSchemes/RecursionSchemes.hs",1],["./haskelldocumentation/exercises/RecursionSchemes/RecursionSchemesSpec.hs",1],["./haskelldocumentation/exercises/Regex/Regex.hs",1],["./haskelldocumentation/exercises/Regex/RegexSpec.hs",1],["./haskelldocumentation/exercises/Spec.hs",null],["./haskelldocumentation/exercises/TextCalculator/TextCalculator.hs",1],["./haskelldocumentation/exercises/TextCalculator/TextCalculatorSpec.hs",1],["./haskelldocumentation/exercises/UpdateRecords/UpdateRecords.hs",1],["./haskelldocumentation/exercises/UpdateRecords/UpdateRecordsSpec.hs",1],["./haskelldocumentation/exercises/Vigenere/Vigenere.hs",1],["./haskelldocumentation/exercises/Vigenere/VigenereSpec.hs",1],["./haskelldocumentation/src/soh-upload/Main.hs",1.3113207547169812],["./haskelldocumentation/src/soh-upload/Preprocess.hs",1.625],["./haskelldocumentation/src/soh-upload/Setup.hs",1]]
```

```sh
localhost:8080/methodComplexity?url=https://github.com/commercialhaskell/haskelldocumentation.git
```
this will give back the project level complexity of the project

```json
["haskelldocumentation",38.50774932614555]
```
