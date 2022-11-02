/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by James Wendel
*/

import 'dart:io';
import 'dart:typed_data';

const String ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

final Frequency IUB = new Frequency([
  'a',
  'c',
  'g',
  't',
  'B',
  'D',
  'H',
  'K',
  'M',
  'N',
  'R',
  'S',
  'V',
  'W',
  'Y'
], [
  0.27,
  0.12,
  0.12,
  0.27,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
]);

final Frequency HOMO_SAPIENS =
    new Frequency(['a', 'c', 'g', 't'], [0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008]);

const int IM = 139968;
const int IA = 3877;
const int IC = 29573;

const int LINE_LENGTH = 60;
const int BUFFER_SIZE = (LINE_LENGTH + 1) * 1024;

const double oneOverIM = (1.0 / IM);

class Frequency {
  late Uint8List chars;
  late Float64List probs;
  late int last;

  double random(double max) {
    last = (last * IA + IC) % IM;
    return max * last * oneOverIM;
  }

  Frequency(List<String> charList, List<double> probList) {
    chars = new Uint8List(charList.length);
    for (int i = 0; i < chars.length; i++) {
      chars[i] = charList[i].codeUnitAt(0);
    }

    probs = new Float64List(probList.length);
    for (int i = 0; i < probList.length; i++) {
      probs[i] = probList[i];
    }

    makeCumulative();
  }

  void makeCumulative() {
    double cp = 0.0;
    for (int i = 0; i < probs.length; i++) {
      cp += probs[i];
      probs[i] = cp;
    }
  }

  int selectRandomIntoBuffer(Uint8List buffer, int bufferIndex, int nRandom) {
    final int len = probs.length;

    outer:
    for (int rIndex = 0; rIndex < nRandom; rIndex++) {
      double r = random(1.0);
      for (int i = 0; i < len; i++) {
        if (r < probs[i]) {
          buffer[bufferIndex++] = chars[i];
          continue outer;
        }
      }

      buffer[bufferIndex++] = chars[len - 1];
    }

    return bufferIndex;
  }
}

makeRepeatFasta(String id, String desc, String alu, int _nChars, IOSink writer) {
  writer.write(">${id} ${desc}\n");

  int aluIndex = 0;
  final List<int> aluCode = alu.codeUnits;
  final int aluLength = aluCode.length;

  Uint8List buffer = new Uint8List(BUFFER_SIZE);

  int bufferIndex = 0;
  int nChars = _nChars;
  while (nChars > 0) {
    final int chunkSize = nChars >= LINE_LENGTH ? LINE_LENGTH : nChars;

    if (bufferIndex == BUFFER_SIZE) {
      writer.add(new Uint8List.view(buffer.buffer, 0, bufferIndex));
      buffer = new Uint8List(BUFFER_SIZE);
      bufferIndex = 0;
    }

    if (aluIndex + chunkSize < aluLength) {
      buffer.setRange(bufferIndex, bufferIndex + chunkSize, aluCode, aluIndex);
      bufferIndex += chunkSize;
      aluIndex += chunkSize;
    } else {
      int len = aluLength - aluIndex;
      buffer.setRange(bufferIndex, bufferIndex + len, aluCode, aluIndex);
      bufferIndex += len;
      aluIndex = 0;
      len = chunkSize - len;
      buffer.setRange(bufferIndex, bufferIndex + len, aluCode, aluIndex);
      bufferIndex += len;
      aluIndex += len;
    }

    buffer[bufferIndex++] = 10;

    nChars -= chunkSize;
  }

  writer.add(new Uint8List.view(buffer.buffer, 0, bufferIndex));
}

void makeRandomFasta(String id, String desc, Frequency fpf, int nChars, IOSink writer) {
  writer.write(">${id} ${desc}\n");

  Uint8List buffer = new Uint8List(BUFFER_SIZE);

  int bufferIndex = 0;
  while (nChars > 0) {
    final int chunkSize = nChars >= LINE_LENGTH ? LINE_LENGTH : nChars;

    if (bufferIndex == BUFFER_SIZE) {
      writer.add(new Uint8List.view(buffer.buffer, 0, bufferIndex));
      buffer = new Uint8List(BUFFER_SIZE);
      bufferIndex = 0;
    }

    bufferIndex = fpf.selectRandomIntoBuffer(buffer, bufferIndex, chunkSize);
    buffer[bufferIndex++] = 10;

    nChars -= chunkSize;
  }

  writer.add(new Uint8List.view(buffer.buffer, 0, bufferIndex));
}

main(args) {
  IOSink writer = stdout;

  int n = args.length > 0 ? int.parse(args[0]) : 250;

  makeRepeatFasta("ONE", "Homo sapiens alu", ALU, n * 2, writer);
  IUB.last = 42;
  makeRandomFasta("TWO", "IUB ambiguity codes", IUB, n * 3, writer);
  HOMO_SAPIENS.last = IUB.last;
  makeRandomFasta("THREE", "Homo sapiens frequency", HOMO_SAPIENS, n * 5, writer);
}
