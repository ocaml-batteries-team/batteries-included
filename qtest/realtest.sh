set -e


./make.sh

modules="../src/batArg.ml ../src/batArray.ml ../src/batAvlTree.ml ../src/batBase64.ml ../src/batBigarray.ml ../src/batBig_int.ml ../src/batBitSet.ml ../src/batBool.ml ../src/batBuffer.ml ../src/batCache.ml ../src/batChar.ml ../src/batCharParser.ml ../src/batComplex.ml ../src/batConcurrent.ml ../src/batCounter.ml ../src/batDeque.ml ../src/batDigest.ml ../src/batDllist.ml ../src/batDynArray.ml ../src/batEnum.ml ../src/batFile.ml ../src/batFloat.ml ../src/batFormat.ml ../src/batGc.ml ../src/batGenlex.ml ../src/batGlobal.ml ../src/batHashcons.ml ../src/batHashtbl.ml ../src/batHeap.ml ../src/batHistogram.ml ../src/batIMap.ml ../src/batInnerIO.ml ../src/batInnerWeaktbl.ml ../src/batInt32.ml ../src/batInt64.ml ../src/batInterfaces.ml ../src/batInt.ml ../src/batIO.ml ../src/batISet.ml ../src/batLazyList.ml ../src/batLexing.ml ../src/batList.ml ../src/batLogger.ml ../src/batLog.ml ../src/batMap.ml ../src/batMarshal.ml ../src/batMultiPMap.ml ../src/batMutex.ml ../src/batNativeint.ml ../src/batNumber.ml ../src/batNum.ml ../src/batOo.ml ../src/batOption.ml ../src/batOptParse.ml ../src/batOrd.ml ../src/batParserCo.ml ../src/batPathGen.ml ../src/batPervasives.ml ../src/batPrintexc.ml ../src/batPrintf.ml ../src/batPrint.ml ../src/batQueue.ml ../src/batRandom.ml ../src/batRefList.ml ../src/batRef.ml ../src/batResult.ml ../src/batReturn.ml ../src/batRMutex.ml ../src/batScanf.ml ../src/batSeq.ml ../src/batSet.ml ../src/batSplay.ml ../src/batStack.ml ../src/batStream.ml ../src/batString.ml ../src/batStr.ml ../src/batSubstring.ml ../src/batSys.ml ../src/batteries.ml ../src/batteriesPrint.ml ../src/batteriesThread.ml ../src/batTuple.ml ../src/batUnit.ml ../src/batUnix.ml ../src/batUref.ml ../src/batValuePrinter.ml ../src/batVect.ml ../src/extlib.ml"

qtest -o outreal.ml --preamble 'open Batteries;;' extract ../src/batString.ml


ocamlbuild -libs bigarray,nums,str -classic-display -cflag -thread -lflag -thread -cflags -warn-error,+26 -use-ocamlfind -package oUnit -Is src,libs outreal.native


./outreal.native