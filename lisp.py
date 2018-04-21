import re
class Cons:
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr
    def __str__(self):
        #try
        return "(" + str(self.car) + " . " + str(self.cdr) + ")"
    def __repr__(self):
        return "(" + repr(self.car) + " . " + repr(self.cdr) +")"
class LispFunction:
    def __init__(self, lambdaexpr, env):
        self.lambdaexpr = lambdaexpr
        self.args = list(iterate(self.lambdaexpr.cdr.car))
        self.returnv = self.lambdaexpr.cdr.cdr.car
        self.env = env
    def __call__(self, *vals):
        newenv = Environment(self.env, parent=self.env)
        newenv.update(zip(self.args,vals))
        return apply(self.returnv, newenv)
        

class Environment(dict):
    def __init__(self, *args, **kwargs):
        self.parent = kwargs.pop('parent')
        dict.__init__(self, *args, **kwargs)

    def innermost(self, var):
        return self if var in self else self.parent.innermost(var)

def syntax_sugar(program):
    new = []
    index = 0
    while index < len(program):
        val = program[index]
        if val == "'":
            new.append(["quote", program[index+1]])
            index +=1
        elif type(val) == list:
            new.append(syntax_sugar(val))
        else:
            new.append(val)
        index += 1
    return recursive_mll(new)
            
            
def make_linked_list(iterable):
    if len(iterable) == 0:
        return None
    current = Cons(None, None)
    a=current
    for i in iterable:
        if current.car == None:
            current.car = i
        else:
            current.cdr = Cons(i, None)
            current = current.cdr
    return a

def recursive_mll(iterable): #ONLY TAKES LIST
    new = []
    for i in iterable:
        if type(i) != list: 
            new.append(i)
        else:
            new.append(recursive_mll(i))
    return make_linked_list(new)
def iterate(linked):
    class LLIterable:
        def __init__(self, linkedlist):
            self.linkedlist = linkedlist
        def __iter__(self):
            class LLIterator:
                def __init__(self, ll):
                    self.current = ll

                def __next__(self):
                    if self.current == None:
                        raise StopIteration
                    this = self.current.car
                    self.current = self.current.cdr
                    return this
            return LLIterator(self.linkedlist)

    return LLIterable(linked)

def append_to_linkedlist(linked, obj):
    current = linked
    while current.cdr != None:
        current = current.cdr
    current.cdr = Cons(obj, None)
    return current

def tokenise(code_str):
    tokenised = []
    token_built = ""
    for i in code_str: 
        if i == "(":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append("(")
        elif i == ")":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(")")
        elif i == " " or i == "\n":
            if (token_built != ""): tokenised.append(token_built) 
            token_built = ""
        elif i == "'":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append("'")
        else:
            token_built += i

    return tokenised


#LIST ::= ( EXPR )
#        |( )
#EXPR ::= ATOM
#        |LIST
#        |EXPR EXPR
#        
def parse(tokenised):
    listy = [[]]
    for token in tokenised:
        if token == "(":
            listy.append([])
        elif token == ")":
            listy[-2].append(listy.pop())
        else:
            try:
                listy[-1].append(int(token))
            except:
                try:
                    listy[-1].append(float(token))
                except:
                    listy[-1].append(token)
    
    return listy[0]


#def e
def func_n_vals(consappl):
    func = consappl.car
    vals = []
    for i in iterate(consappl.cdr):
        vals.append(i)
    return func, vals

def apply(consappl,localvars):
    if consappl.car == 'quote':
        return consappl.cdr.car
    func, vals = func_n_vals(consappl)
    if func == 'cond':
        for cons in vals:
            if evaluate(cons.car,localvars) != None:
                return evaluate(cons.cdr.car,localvars)
        raise Exception('no default in cond, and it fell through!!')
    elif func == 'lambda': 
        return LispFunction(consappl,localvars)
    elif func == 'define':
        localvars[vals[0]] = evaluate(vals[1], localvars)
        return localvars[vals[0]]
    elif func == 'set!':
        localvars.innermost(vals[0])[vals[0]] = evaluate(vals[1], localvars)
    else: #defined
        func = evaluate(func, localvars)
        vals = [evaluate(val,localvars) for val in vals]
        return func(*vals) #for builtins
def evaluate(expr,localvars):
    if type(expr)== Cons:
        return apply(expr,localvars)
    else:
        if type(expr) == str:
            return localvars.innermost(expr)[expr]#variables!
        else:
            return expr

def defaultenv():
    env = Environment({}, parent=None)
    def eq(val0, val1):                              #MCCARTHY PAPER VERSION
        if type(val0) == Cons or type(val1) == Cons:
            raise Exception("eq doesn't take a cons")
        return "t" if val0 == val1 else None #eval?
    def begin(*vals):
        return vals[-1]
    env.update([
        ('eq?', eq), #('eq?', (lambda x,y: x is y), #SCHEME VERSION (CURRENTLY COMMENTED OUT)
        ('car', (lambda x: x.car)),
        ('cdr', (lambda x: x.cdr)),
        ('cons', (lambda x, y: Cons(x, y))),
        ('atom?', (lambda x: not isinstance(x, Cons))),
        ('begin', begin),
        ('write', (lambda x: print(x) or x)), #dirty hack -- print always returns None :. this always returns x, but prints x first.
        ('+', (lambda x, y: x+y)),
        ('-', (lambda x, y: x-y)),
        ('*', (lambda x, y: x*y)),
        ('/', (lambda x, y: x/y))
    ])
    return env

globals = defaultenv()
def execute(parsed):
    for i in iterate(parsed):
        apply(i, globals)

def run(program):
    execute(syntax_sugar(parse(tokenise(program))))

#((lambda (x) (cond ((eq (quote 1) x) (quote t)) ((eq (quote 1) x) (quote f)))) (quote 1))
if __name__ == "__main__":
    test="""
(define fibonacci
 (lambda (n)
  (cond
   ((eq? n 1) 0)
   ((eq? n 2) 1)
   ('t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
  )
 )
)

(write (fibonacci 5))
"""
    run(test)
