Samuel Carnes
CSCI 3155 Lab 2
Team Members - Samuel Carnes, Dillon Drenzek

1. Grammars: Synthetic Examples
	(a) Consider the following grammar:
			A ::= A&A|V
			V ::= a|b
		
		Axioms:
			a ∈ VObjects        b ∈ VObjects
			
		-------------------------------------------	
			
			s1 ∈ AObjects                    S ∈ VObjects
		-------------------------------------------
			s1 & s2 ∈ AObjects         S ∈ AObjects
			
	(b) Show that the grammar in the previous part is ambiguous
	
			Due to the order of the input being unspecified, the grammar is ambiguous. This would mean that the same input could get different results, or you could have different inputs give the same results.
			For example, say we're looking for a parsing tree generating the string "a&a&a" in both cases. This can be broken due to it being possible for V to translate to either a or b.
			
						A						A
					   /|\					   /|\
					  A & A					  A & A
					 /   /|\				 /|\   \
					V	A & A				A & A   V
			
			Seeing as how it's possible for more than one parse tree to occur in relation to a single string, this is what qualifies the case as being ambiguous.
			
	(c) Describe the language defined by the following grammar:
		S ::= A|B|C
		A ::= aA|a
		B ::= bB|ε
		C ::= cC|c
		
			L(S) = {a^n, b^m, c^n; n > 0 & m >= 0}
			
			L(A) = {a^n; n >= 1}
			L(B) = {b^n; m >= 0}
			L(C) = [c^n; n >= 1}
		
		A and C are always going to run n + 1 times, an a or a c always being present in the terminal.
		B however will only run n times due to having an empty terminal.
		
	(d) Consider the following grammar:
									S ::= AaBb
									A ::= Ab|b
									B ::= aB|a
		1. baab
			
			S ::= A a B b
				= b a B b
				= b a a b = baab
				TRUE
		
		2. bbbab
		
			S ::= Ab a B b
				= Abb a B b
				= bbb a B b
				= bbb a Ab|b
				This is an impossible move as picking Ab for the end results in too many b's
				FALSE
				
		3. bbaaaaa
		
			The terminal must end with a 'b' statement as it did above
			FALSE
			
		4. bbaab
			S ::= A b B b
				= A b aB b
				= b b a a b = bbaab
				
	(e) Consider the following grammar:
									S ::= aScB|A|b
									A ::= cA|c
									B ::= d|A
		Which of the following sentences are generated by this grammar?
		For the sentences that are described by this grammar, give parse trees.
		
		1. abcd
		
				S
			  /| |\
			 a S c B
			   |   |
			   b   d
		
		2. acccbd
		
			This is not possible in the grammar as a "bd" cannot be formed
			
		3. acccbccc
			
			This isn't possible either in the grammar as a "cb" can't be formed
		
		4. acd
			
			The only means of getting 'a' anywhere requires four letters
			
		5. accc

				S
			  /| |\
			 a S c B
			   |   |
			   A   A
			   |   |
			   c   c
			   
2. Grammars: Understanding a Language
	(a) Consider the following two grammars for expressions e.
					
					e ::= operand|e operator operand
					
					e ::= operand esuffix
			  esuffix ::= operator operand esuffix|ε
		
		i. While being produced by different means, they are both operands determined by binary operators.
		
		ii. It's possible for them to, but since they build themselves in opposite directions (the first using left associativity while the other uses right associativity, expanding in their associated directions), they will not be giving the same expression ultimately.
			The one with left associativity will construct itself from the end at e operator operand, moving on from the beginning of the structure. In an opposite manner, the right associated one will begin with an operand, and construct things onto it following the essuffix.

	(b) Going by the knowledge of << x equating to multiplying by 2^x, we can run tests based on order of operations:
		
		1 << 1 - 1
		Returns 1
		
		1 << 2 - 1
		Returns 2
		
		2 - 1 << 1
		Returns 2
		
		Running the aforementioned order of operations tests:
		
		(1 << 1) - 1
		Returns 1
		
		(1 << 2) - 1
		Returns 3
		
		2 - (1 << 1)
		Returns 0
		
		Since the order of operations appears to be getting broken, it would seem that '-' takes a higher priority that '<<'.
	
	(c) S ::= NF.nx
		F ::= d.n | ε
		x ::= ENdn | ε
		N ::= - | ε
		n ::= zN | dN | ε
		z ::= 0
		d :: = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9