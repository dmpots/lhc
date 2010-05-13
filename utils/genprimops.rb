#!/usr/bin/ruby
require 'pp'

class Prim
  attr_accessor :name, :result, :length
  def initialize(name)
    @name = name
    @result = :unknown
    @length = 0
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
    if (prim.result == :vector && prim.length == 0) then
      error("vector type with length 0: #{prim.name}")
    end
  end
end
def addPrimType(prims, name, type, length=0)
  prim = prims[name]
  if prim.nil? then
    error("trying to add type for unknown prim: #{name}")
  elsif (prim.result != :unknown) then
    error("multiple types for prim: #{name}")
  elsif (prim.name != name)
    error("name mismatch for primitive: #{name} != #{prim.name}")
  else
    prim.result = type
    prim.length = length
    debug("PRIM: #{prim.name} (#{prim.result}, #{prim.length})")
  end
end

PRIM_FILE = "lib/ghc-prim/GHC/Prim.hs" 
prims = {}
ghcBuiltins = []

# Collect the name of all primitive operations
File.open(PRIM_FILE).each do |line|
  if line =~ /#,$/ then
    op = line.gsub(",","").lstrip.rstrip
    ghcBuiltins << op
    prims[op] = Prim.new(op)
  end
end

# Find the return type (base, vector) of each primitive
File.open(PRIM_FILE).each do |line|
  if line =~ /(\w+\#)\s+::\s+.*\(\#(.*)\#\)\s*$/ then
    name,len = $1,$2.split(',').length
    debug("V: #{line.chomp} YIELDS: #{name} of length #{len}")
    addPrimType(prims, name, :vector, len)
  elsif line =~ /(\w+\#)\s+::\s+(.*)$/ then
    name = $1
    debug("B: #{line}")
    addPrimType(prims, name, :base)
  elsif line =~ /data (\w+\#)(\s*[a-z])*$/ then
    name = $1
    debug("C: #{line}")
    addPrimType(prims, name, :base)
  else
    debug("JUNK: #{line}")
  end
end

# Perform sanity checks on the parsed prims
sanity(prims)

unsupportedBuiltins = %w(
  raise#
  atomicModifyMutVar#
  writeTVar#
  raiseIO#
  fork#
  atomically#

  makeStablePtr#
  deRefStablePtr#
  unblockAsyncExceptions#
  blockAsyncExceptions#
  newArray#
  readArray#
  unsafeFreezeArray#
  indexArray#
  writeArray#
  mkWeak#
  deRefWeak#
)

specialBuiltins = %w(
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
vb             = prims.values.find_all {|p| p.result == :vector}
vectorBuiltins = vb.reject{|p| specialCases.index(p.name)}

def printVectorList(name, values)
  puts "#{name} :: [(CompactString, Int)]"
  puts "#{name} = ["
  print "    "
  puts values.map{|v| '(' + v.name.sub(/^/, '"').sub(/$/, '"') + ',' + v.length.to_s + ')'}.join("\n  , ")
  puts "  ]"
end
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
#printList("vectorBuiltins", vectorBuiltins.map{|v|v.name}.sort)
printVectorList("vectorBuiltins", vectorBuiltins.sort_by{|v|v.name})
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


