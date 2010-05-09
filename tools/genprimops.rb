#!/usr/bin/ruby

class Prim
  attr_accessor :name, :result
  def initialize(name)
    @name = name
    @result = :unknown
  end
end

def error(msg)
  puts "ERROR: #{msg}"
  exit 1
end
def debug(msg)
  #puts msg
end
def sanity(prims)
  prims.values.each do |prim| 
    if (prim.result != :vector && prim.result != :base) then
      error("unknown prim type (#{prim.result}) for: #{prim.name}")
    end
  end
end
def addPrimType(prims, name, type)
  prim = prims[name]
  if prim.nil? then
    error("trying to add type for unknown prim: #{name}")
  elsif (prim.result != :unknown) then
    error("multiple types for prim: #{name}")
  else
    prim.result = type
  end
end

PRIM_FILE = "lib/ghc-prim/GHC/Prim.hs" 
prims = {}
ghcBuiltins = []
File.open(PRIM_FILE).each do |line|
  if line =~ /#,$/ then
    op = line.gsub(",","").lstrip.rstrip
    ghcBuiltins << op
    prims[op] = Prim.new(op)
  end
end

File.open(PRIM_FILE).each do |line|
  if line =~ /(\w+\#)\s+::\s+(.*)\#\)\s*$/ then
    debug("V: #{line}")
    addPrimType(prims, $1, :vector)
  elsif line =~ /(\w+\#)\s+::\s+(.*)$/ then
    debug("B: #{line}")
    addPrimType(prims, $1, :base)
  elsif line =~ /data (\w+\#)(\s*[a-z])*$/ then
    debug("C: #{line}")
    addPrimType(prims, $1, :base)
  else
    debug("JUNK: #{line}")
  end
end

sanity(prims)

unsupportedBuiltins = %w(
  raise#
  atomicModifyMutVar#
  writeTVar#
  raiseIO#
  fork#
  atomically#
)

specialBuiltins = %w(
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

specialCases   = specialBuiltins + unsupportedBuiltins
bb             = prims.values.find_all {|p| p.result == :base}.map   {|p| p.name}
baseBuiltins   = bb + extraBuiltins - specialCases
vb             =  prims.values.find_all {|p| p.result == :vector}.map {|p| p.name}
vectorBuiltins = vb - specialCases

def printList(name, values)
  puts "#{name} :: [CompactString]"
  puts "#{name} = ["
  print "    "
  puts values.map{|v| v.sub(/^/, '"').sub(/$/, '"')}.join("\n  , ")
  puts "  ]"
end

puts "{-# LANGUAGE OverloadedStrings #-}"
puts "module LHC.Prim where"
puts "import CompactString"
printList("baseBuiltins", baseBuiltins.sort)
printList("vectorBuiltins", vectorBuiltins.sort)
printList("unsupportedBuiltins", unsupportedBuiltins.sort)

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


