/* BASIC ARITMETIC */
1 + 2;
1 - 2;
1 * 2;
1 / 2;
1 ^ 2;
1 % 2;
1 + 2 - 3 * 4 / 5 ^ 6 % 7;
1 + (2 - (3 * (4 / 5) ^ 6) % 7);

/* INCREMENTERS & DECREMENTERS */
1++;
1--;
((3++) + (4--))++;

/* BIT SHIFTING & ROTATING */
1 << 2;
1 >> 2;
1 RotL 2;
1 RotR 2;
1 << (4 RotL 5) >> (3 RotR (7 >> 2));

/* BINARY ANDING / ORING */
1 &   2;
1 |   2;
1 xor 2;
(1 xor 4) & (2 | 3);

/* UNARY ARITHMETIC EXPRESSIONS (COMPLEMENT & NEGATION) */
-1;
~1;
-(1 + ~(2 + 3));

/* BOOLEAN EXPRESSIONS */
1 == 2;
1 != 2;
1 >  2;
1 >= 2;
1 <  2;
1 <= 2;
(3 + 4) == (2 * 5);

/* BOOLEAN ANDING / ORING */
True && False
True || False
True XOR False;
(True && False) || (True XOR False);

/* ASSIGNMENTS, TYPED ASSIGNMENTS, CHAINED ASSIGNMENTS */
a = 1;
b = 1 + 2 * 3 / 4 ^ 5 % 6;
c = d = e = f = (4 + 2);

printLn ("A: " + a);
a = "Wazzap";
printLn ("A: " + a);

typeInt g;
g = 1;
g = True;
printLn ("G: " + g);

readOnly typeStr h = "Hello";
h = "World!";
printLn ("H: " + h);

typeInt i = j = k = 100;
printLn ("J: " + j);

typeInt typeStr l = 5;
printLn ("L: " + l);
l = "Hello!";
printLn ("L: " + l);
l = True;
printLn ("L: " + l);

/* DEALLOCATE VARIABLE */
delete i;

/* ARRAYS */
typeInt[] arr1 = {1, 2, 3};
arr2 = {1, 2.2, True, "Hello", 'w', arr1}
arr1[5] = 10;
getVal = arr1[2];



/* APPLY TO SELF */
g += 2;
g -= 1;
g *= 2;
g /= 2;
g ^= 4;
printLn ("G: " + g);

/* LOCALIZED ENVIORMENTS */
aj = 5;
printLn ("AJ: " + aj);
{
    ak = 100;
    aj *= 2;
    printLn ("AK: " + ak);

    {
        al = 10;
        ak /= 2;
        printLn ("AL: " + al);
    }

    printLn ("AL: " + al);
    printLn ("AK: " + ak);
}
printLn ("AJ: " + aj);
printLn ("AK: " + ak);
printLn ("AL: " + al);

/* TERNARY IF STATEMENT */
am = ((1 > 2) ? "Option 1" : "Option 2");
typeInt typeStr typeBool an = ((1 > 2) ? 1 : ((1 > 2) ? "Option 2" : True));
printLn ("AM: " + am);
printLn ("AN: " + an);

/* IF WITHOUT ELSE CLAUSE */
if (True) {
    printLn ("If using constant");
}
ao = True;
ap = False
if (ao || ap) {
    printLn ("If using boolean expression");
    if (ao && ap) {
        printLn ("Nested if");
    }
}
aq = 1;
if (aq) {
    printLn ("If without boolean case");
}

/* IF WITH ELSE CLAUSE */
if (False) {
    printLn ("If with else");
} else {
    printLn ("Else block");
}

if (False) {
    printLn ("Block 1");
} else if (False) {
    printLn ("Block 2");
} else {
    printLn ("Block 3");
}

/* SWITCH STATMENTS */
typeInt ar = 3;
switch (ar) {
    case 1 {
        printLn ("One");
    }
    case 2 {
        printLn ("Two");
    }
    default {
        printLn ("Unknown");
    }
}

/* FOR LOOPS */
for (typeInt as = 0; as < 10; as++) { 
    if (as % 2 == 0) {
        printLn ("AS: " + as);
    }
}

for ({ at = 0; typeStr au = "" }; (at < 10); { at++; au = au + "," +(typeStr) at}) {
    printLn ("AT: " + at);
    printLn ("AU: " + au);
}

typeInt ba;
for typeStr bb in "Hello World!" {
    print bb;
    ba++;
}

printLn "";
printLn ba;

/* WHILE / DO WHILE LOOPS */
av = 0;
while (av < 5) { 
    printLn ("AV: " + av);
    av++;
}

/* TYPE INFRENCE & TYPE CASTING */
typeStr typeInt ca = 1;
typeStr typeInt cb = "Hello";
printLn ("CA is of type: " + (type ca));
printLn ("CB is of type: " + (type cb));

typeStr cx = "10";
typeInt cc = (typeInt) cx;
typeDbl cd = (typeDbl) cx;
typeBol ce = (typeBol) cx;
typeStr cf = (typeStr) cx;
typeChr cg = (typeChr) cx;


/* NON RETURNING FUNCTIONS */
def funcTest(typeInt da = 5, typeStr db = "World") {
    printLn ("DA: " + da);
    printLn ("DB: " + db);
}

funcTest();
funcTest(10, "Hello");

/* RETURNING FUNCTIONS */
typeInt def product(typeInt x, typeInt y) {
    printLn ("X: " + x);
    printLn ("Y: " + y);
    return = (x * y);
}
typeInt prod = product(5, 10);
printLn ("Product: " + prod);

/* RECURSIVE FUNCTIONS */
def recurse(typeInt x) {
    if (x >= 0) {
        printLn ("X: " + x);
        recurse(x - 1);
    }
}
recurse(5);

/* CLASSES */
public class Person {
    private typeStr name;
    public typeInt age;
    public typeObj Pet myPet;

    public def create(typeStr n, typeInt a) {
        name = n;
        age = a;
        myPet = new Pet("Freya", 9);
    }

    public def setName(typeStr n) {
        name = n;
    }

    public def setAge(typeInt a) {
        printLn "Setting age...";
        age = a;
    }

    public typeInt def getAge() {
        return = age;
    }

    public typeStr def getName() {
        printLn "Getting age...";
        return = name;
    }

    public def toStr() {
        printLn ("My name is '" + name + "', and my age is: " + age);
    }

    private def secretFunction() {
        printLn "This can only be executed from inside the class!";
    }

    public class Pet {
        public typeStr petName;
        private typeInt petAge;

        public def create(typeStr pn = "Brett", typeInt pa = 57) {
            petName = pn;
            petAge = pa;
        }

        public def getAge() {
            return = petAge;
        }

        public def setAge(typeInt pa = 30) {
            petAge = pa;
        }
    }
}

typeObj Person myP = new Person("Matthew", 24);

myName = myP.name;
myAge = myP.age;

printLn ("Name: " + myName);
printLn ("Age: " + myAge);

myName = myP.getName();
printLn ("Name: " + myName);

myP.setAge(50);
myAge = myP.getAge();
printLn ("Age: " + myAge);

myP.age = 100;
myAge = myP.age;
printLn ("Age: " + myAge);

myP.toStr();
myP.secretFunction();

myPetName = myP.myPet.petName;
myPetAge = myP.myPet.petAge;
printLn ("Pet Name: " + myPetName);
printLn ("Pet Age: " + myPetAge);

myPetAge = myP.myPet.getAge();
printLn ("Pet Age: " + myPetAge);

myP.myPet.setAge(17);
myPetAge = myP.myPet.getAge();
printLn ("Pet Age: " + myPetAge);

myP.myPet.petName = "Butter";
myPetName = myP.myPet.petName;
printLn ("Pet Name: " + myPetName);