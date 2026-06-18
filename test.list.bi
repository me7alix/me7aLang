:i count 26
:b shell 95
./build/release/m7c -tc nasm64 -O0 -I ./stdlib -o build/prog examples/bubble.m7 && ./build/prog
:i returncode 0
:b stdout 61
0: 0
1: 2
2: 8
3: 20
4: 23
5: 56
6: 78
7: 110
8: 153
9: 2410

:b stderr 0

:b shell 95
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/bubble.m7 && ./build/prog
:i returncode 0
:b stdout 61
0: 0
1: 2
2: 8
3: 20
4: 23
5: 56
6: 78
7: 110
8: 153
9: 2410

:b stderr 0

:b shell 96
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/ifchain.m7 && ./build/prog
:i returncode 0
:b stdout 90
a 0
a 1
a 2
a 3
a 4
  5
  6
  7
b 8
b 9
b 10
b 11
b 12
  13
  14
X 15
  16
  17
  18
  19

:b stderr 0

:b shell 100
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/dynarr_test.m7 && ./build/prog
:i returncode 0
:b stdout 62
count: 3
capacity: 256

first: 69
last: 78

1) 70
2) 56
3) 90

:b stderr 0

:b shell 101
./build/release/m7c -tc nasm64 -O0 -I ./stdlib -o build/prog examples/hashmap_test.m7 && ./build/prog
:i returncode 0
:b stdout 28
one: (nil)
ten	10
twenty	20

:b stderr 0

:b shell 101
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/hashmap_test.m7 && ./build/prog
:i returncode 0
:b stdout 28
one: (nil)
ten	10
twenty	20

:b stderr 0

:b shell 97
./build/release/m7c -tc nasm64 -O0 -I ./stdlib -o build/prog examples/pointers.m7 && ./build/prog
:i returncode 0
:b stdout 126
arr1:
0: 0
1: 1
2: 4
3: 9
4: 16
5: 25
6: 36
7: 49
8: 64
9: 81

arr2:
0: 0
1: 1
2: 4
3: 9
4: 111
5: 25
6: 36
7: 49
8: 64
9: 81

:b stderr 0

:b shell 97
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/pointers.m7 && ./build/prog
:i returncode 0
:b stdout 126
arr1:
0: 0
1: 1
2: 4
3: 9
4: 16
5: 25
6: 36
7: 49
8: 64
9: 81

arr2:
0: 0
1: 1
2: 4
3: 9
4: 111
5: 25
6: 36
7: 49
8: 64
9: 81

:b stderr 0

:b shell 94
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/bwops.m7 && ./build/prog
:i returncode 0
:b stdout 129
XOR swap
6 9
9 6

Bit shifts
09 << 01 = 18
09 >> 01 = 04

Bitwise and/or
09 & 06 = 00
09 | 06 = 15

Bitwise not
~9 = -10
~6 = -7

:b stderr 0

:b shell 96
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/structs.m7 && ./build/prog
:i returncode 0
:b stdout 53
Name: Michael
Salary: 777
Address: New York [100101]

:b stderr 0

:b shell 104
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/fib.m7 && echo '10' | ./build/prog
:i returncode 0
:b stdout 30
cnt: 1
2
3
5
8
13
21
34
55
89

:b stderr 0

:b shell 104
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/snake.m7 -lf /usr/lib/libraylib.so
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 126
./build/release/m7c -tc nasm64 -O1 -I ./stdlib -o build/prog examples/transunits/a.m7 examples/transunits/b.m7 && ./build/prog
:i returncode 0
:b stdout 13
Hello World!

:b stderr 0

:b shell 95
./build/release/m7c -tc fasm64 -O0 -I ./stdlib -o build/prog examples/bubble.m7 && ./build/prog
:i returncode 0
:b stdout 61
0: 0
1: 2
2: 8
3: 20
4: 23
5: 56
6: 78
7: 110
8: 153
9: 2410

:b stderr 0

:b shell 95
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/bubble.m7 && ./build/prog
:i returncode 0
:b stdout 61
0: 0
1: 2
2: 8
3: 20
4: 23
5: 56
6: 78
7: 110
8: 153
9: 2410

:b stderr 0

:b shell 96
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/ifchain.m7 && ./build/prog
:i returncode 0
:b stdout 90
a 0
a 1
a 2
a 3
a 4
  5
  6
  7
b 8
b 9
b 10
b 11
b 12
  13
  14
X 15
  16
  17
  18
  19

:b stderr 0

:b shell 100
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/dynarr_test.m7 && ./build/prog
:i returncode 0
:b stdout 62
count: 3
capacity: 256

first: 69
last: 78

1) 70
2) 56
3) 90

:b stderr 0

:b shell 101
./build/release/m7c -tc fasm64 -O0 -I ./stdlib -o build/prog examples/hashmap_test.m7 && ./build/prog
:i returncode 0
:b stdout 28
one: (nil)
ten	10
twenty	20

:b stderr 0

:b shell 101
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/hashmap_test.m7 && ./build/prog
:i returncode 0
:b stdout 28
one: (nil)
ten	10
twenty	20

:b stderr 0

:b shell 97
./build/release/m7c -tc fasm64 -O0 -I ./stdlib -o build/prog examples/pointers.m7 && ./build/prog
:i returncode 0
:b stdout 126
arr1:
0: 0
1: 1
2: 4
3: 9
4: 16
5: 25
6: 36
7: 49
8: 64
9: 81

arr2:
0: 0
1: 1
2: 4
3: 9
4: 111
5: 25
6: 36
7: 49
8: 64
9: 81

:b stderr 0

:b shell 97
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/pointers.m7 && ./build/prog
:i returncode 0
:b stdout 126
arr1:
0: 0
1: 1
2: 4
3: 9
4: 16
5: 25
6: 36
7: 49
8: 64
9: 81

arr2:
0: 0
1: 1
2: 4
3: 9
4: 111
5: 25
6: 36
7: 49
8: 64
9: 81

:b stderr 0

:b shell 94
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/bwops.m7 && ./build/prog
:i returncode 0
:b stdout 129
XOR swap
6 9
9 6

Bit shifts
09 << 01 = 18
09 >> 01 = 04

Bitwise and/or
09 & 06 = 00
09 | 06 = 15

Bitwise not
~9 = -10
~6 = -7

:b stderr 0

:b shell 96
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/structs.m7 && ./build/prog
:i returncode 0
:b stdout 53
Name: Michael
Salary: 777
Address: New York [100101]

:b stderr 0

:b shell 104
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/fib.m7 && echo '10' | ./build/prog
:i returncode 0
:b stdout 30
cnt: 1
2
3
5
8
13
21
34
55
89

:b stderr 0

:b shell 104
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/snake.m7 -lf /usr/lib/libraylib.so
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 126
./build/release/m7c -tc fasm64 -O1 -I ./stdlib -o build/prog examples/transunits/a.m7 examples/transunits/b.m7 && ./build/prog
:i returncode 0
:b stdout 13
Hello World!

:b stderr 0

