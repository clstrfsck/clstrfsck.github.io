module d03;

import std;

pure @safe long helper(const int[] currentBank, int digits, long acc) {
  if (digits == 0)
    return acc;
  const maxIndex = currentBank[0 .. $ - (digits - 1)].maxIndex;
  const bestJolt = currentBank[maxIndex];
  const remainingBank = currentBank[maxIndex + 1 .. $];
  return helper(remainingBank, digits - 1, acc * 10 + bestJolt);
}

pure @safe long getMaxJolts(int[][] data, int count) {
  return data
    .map!(bank => helper(bank, count, 0))
    .sum;
}

pure @safe int[][] parseData(string content) {
  return content
    .splitLines
    .map!(line => line.strip.map!(c => c - '0').array.to!(int[]))
    .array;
}

void main() {
  enum fileContent = import("y25d03.txt");
  enum banks = fileContent.parseData();
  enum result1 = banks.getMaxJolts(2);
  enum result2 = banks.getMaxJolts(12);

  writeln("Result1: ", result1);
  writeln("Result2: ", result2);
}