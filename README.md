## Problem Statement
The problem aims to implement a package for the big numbers in SML(functional programing language). Given the signature of the package we need to implement the structure and further using the concept of functor, need to generalize it for the signed integers.

## Abstract of Development Phase
We learned the SML(functional Programming language) as a part of our course, using the concept of sml we implemented the functions specified in the signature of package. Further concept of functor was used to generalize it for the negative numbers.

## Instruction to test Bignat
1. open Bignat
>After this we can access all function of Bignat.
2. val a= fromString(String);
3. Now 'a' will be type of Bignat.
4. Now we can apply any constructor of structure of Bignat.


## Instruction to test Bigint
1. open Bigint
After this we can access all function/operator of struture(**Bigint**) produced by applying **functor** on structure **BigInt**.
2. val b=(type 'a int,type 'a Bignat);
3. val c=normalize(b);
Now 'c' will be of type bigint.
4. I was unable to implement constructor *bigint* which takes *int* as input as convert to *bigint*.I was unable to do so as my type of bigint is of int*Bignat. Here *int* is used to indicate sign bit. 
5. But now all operator of structure **Bigint** can be operated over 'c'. 


## Assumption
1. multiply(**) instruction is not used as infix operator.
2. Operator like ++,--,>>=,<<=,>>,<<, are used as infix operator.
3. I was unable to mention ** as infix operator.In line no. 480-486 I forgot to mention ** as infix operator. Line 480-486 is mentioned below:
>infix ==;
>infix >>=;
>infix >>;
>infix <<=;
>infix <<;
>infix --;
>infix ++;
It can be matched to my code.
4. But my multiplication operand is working correctly.It can be used as **(operand1,operand2).
5. But if it is the case that Test case include ** as infix operator then my program will not able to multiplication.Also it was not mentioned in description of assignment that we had to implement ** as infix operator.
6. I am using some operator as infix while only multiplication operator(**) in another form.




