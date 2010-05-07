#!/usr/bin/ruby



ghcBuiltins = []
File.open("lib/ghc-prim/GHC/Prim.hs").each do |line|
  if line =~ /#,$/ then
    op = line.gsub(",","").lstrip.rstrip
    ghcBuiltins << op
  end
end


vectorBuiltins = %w(
  unsafeFreezeByteArray#
  newAlignedPinnedByteArray#
  word2Integer#
  integer2Int#
  newByteArray#
  newPinnedByteArray#
                      
  readInt8OffAddr#
  readInt32OffAddr#
  readAddrOffAddr#
  readInt32OffAddr#

  readWord8Array#
  readDoubleOffAddr#
  writeDoubleOffAddr#
                        
  mkWeak#
  readCharArray#
)

unsupportedBuiltins = %w(
  raise#
  atomicModifyMutVar#
  writeTVar#
  raiseIO#
  fork#
  atomically#
)

specialPrims = %w(
  makeStablePtr#
  deRefStablePtr#
  unblockAsyncExceptions#
  blockAsyncExceptions#
  newArray#
  readArray#
  unsafeFreezeArray#
  indexArray#
  writeArray#
)

extraBuiltins = %w(
  <#
  >#
  <=#
  >=#
  -#
  +#
  *#
  /=#
  ==#
  <##
  ==##
  >##
  <=##
  >=##
  -##
  +##
  *##
  /##
  realWorld#
)


def printList(name, values)
  puts "#{name} = ["
  print "    "
  puts values.map{|v| v.sub(/^/, '"').sub(/$/, '"')}.join("\n  , ")
  puts "  ]"
end

puts "{-# LANGUAGE OverloadedStrings #-}"
puts "module LHC.Prim where"
puts "import CompactString"
puts "allBuiltins, baseBuiltins, vectorBuiltins, unsupportedBuiltins :: [CompactString]"
allBuiltins = ghcBuiltins + extraBuiltins
printList("allBuiltins", allBuiltins)
printList("baseBuiltins", allBuiltins - vectorBuiltins - unsupportedBuiltins - specialPrims)
printList("vectorBuiltins", vectorBuiltins)
printList("unsupportedBuiltins", unsupportedBuiltins)

puts "{- ORIGINAL DEFINITIONS"
puts <<ORIG
baseBuiltins        = ["<#",">#","<=#",">=#","-#","+#","*#","narrow32Int#"
                      ,"uncheckedIShiftRA#","and#","==#", "remInt#", "noDuplicate#"
                      ,"narrow8Word#", "writeInt8OffAddr#", "writeWord8OffAddr#"
                      ,"narrow8Int#", "byteArrayContents#","touch#"
                      ,"uncheckedIShiftL#", "negateInt#", "not#"
                      ,"indexCharOffAddr#","minusWord#","geWord#","eqWord#","narrow16Word#"
                      ,"neWord#", "ltWord#", "gtWord#", "remWord#"
                      ,"ord#","chr#","or#","narrow32Word#","uncheckedShiftL#","plusWord#"
                      ,"uncheckedShiftRL#","neChar#","narrow16Int#","timesWord#"
                      ,"writeAddrOffAddr#","writeInt32OffAddr#","quotInt#", "quotWord#"
                      ,"writeDoubleOffAddr#"
                      ,"readWord32OffAddr#"
                      ,"writeWord32OffAddr#"
                      ,"waitRead#"
                      ,"leWord#","/=#","writeCharArray#","xor#", "realWorld#"
                      ,"waitWrite#", "negateDouble#", "negateFloat#", "sqrtDouble#", "expDouble#", "**##"
                      ,"sinDouble#", "tanDouble#", "cosDouble#", "asinDouble#", "atanDouble#"
                      ,"acosDouble#", "asinhDouble#", "sinhDouble#", "tanhDouble#", "coshDouble#"
                      ,"<##", "==##", ">##", "<=##", ">=##", "-##", "+##", "*##", "/##"
                      ,"ltFloat#", "eqFloat#", "writeWord8Array#"
                      ,"coerceDoubleToWord", "coerceWordToDouble", "logDouble#", "int2Double#", "double2Int#"
                      ,"int2Float#", "divideFloat#", "timesFloat#", "minusFloat#", "plusFloat#"
                      ,"gtFloat#", "geFloat#", "leFloat#", "sqrtFloat#" ]
vectorBuiltins      = ["unsafeFreezeByteArray#", "newAlignedPinnedByteArray#"
                      , "word2Integer#","integer2Int#", "newByteArray#", "newPinnedByteArray#"
                      ,"readInt8OffAddr#","readInt32OffAddr#","readAddrOffAddr#","readInt32OffAddr#"
                      ,"readWord8Array#", "readDoubleOffAddr#", "writeDoubleOffAddr#"
                      ,"mkWeak#", "readCharArray#"]
unsupportedBuiltins = ["raise#","atomicModifyMutVar#","writeTVar#"
                      ,"raiseIO#","fork#","atomically#"]
ORIG
puts "-}"


