#Lispin. A lisp interpreter.
#    Copyright (C) 2018  e-dt
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

##TODO:
##* hygienic macros using in_env etc.
##*
##* dynamic variables
##  pass around a `dynvars` variable
##  dynmacros?
##  will complicate environment functions, but not much
##
import copy
from types import FunctionType, BuiltinFunctionType
def pretty_print(lst):
    new = []
    for i in iterate(lst):
        if type(i) != Cons: 
            new.append(i)
        else:
            new.append(pretty_print(i))
    return new
def pretty(lt):
    #for debugging purposes only
    print(pretty_print(lt))
class Cons: #Pair object
    """A pair."""
    def __init__(self, car, cdr):
        self.car = car #first
        self.cdr = cdr #rest (in linked list)
    def __str__(self):
        return "(" + str(self.car) + " . " + str(self.cdr) + ")"
    def __repr__(self):
        return "(" + repr(self.car) + " . " + repr(self.cdr) +")"
class Vunction:
    """A callable lisp function."""
    def __init__(self, body, env):
        self.body = body #get body of self
        self.args = self.body.cdr.car
        self.dynamic = self.body.cdr.cdr.car
        #cdr.car (lambda (x) ...) = (x)
        self.returnv = self.body.cdr.cdr.cdr.car
        #cdr.cdr.car of (lambda (x y) (do x y z...))
        #is cdr.car of ((x y) (do x y z)) is (do x y z)
        self.env = env #Le epic lexical scoping
        
        #PLEASE NOTE -- lambdas only take one expression.
        #if you want to use more than one, use a (begin ...)
        
    def __call__(self, dynamic, vals):
        newenv = Environment(self.env, parent=self.env)
        if type(self.args) == Symbol: #pass cdr function
            newenv[self.args] = vals
        elif type(self.args) == Cons:
            old = newenv
            newenv.update(zip(iterate(self.args), iterate(vals)))
        #defines the arguments' values to be the values passed in
        if self.dynamic != None:
            newenv[self.dynamic] = dynamic
        #has extra arg, for caller's env 
        return evaluate(self.returnv, newenv)
    
    def __repr__(self):
        return "vunc" #debugging
        

class Environment(dict):
    def __init__(self, *args, **kwargs):
        self.parent = kwargs.pop('parent') #parent is the parent environment, e.g. global environment
        dict.__init__(self, *args, **kwargs)

    def innermost(self, var):
        """Gets innermost environment in which `var` appears."""
        try:
            return self if var in self else self.parent.innermost(var)
        except:
            raise NameError("%s not defined in environment." % var)

class Symbol(str):
    pass #is the same

class Token(str):
    pass #is the same

def syntax_sugar(program):
    new = []
    index = 0
    while index < len(program):
        val = program[index]
        if val == Token("'"): #'x -> (quote x)
            new.append([Symbol("quote"), program[index+1]])
            index += 1 #skip next value, as it is already used in the quote expr
        elif type(val) == list:
            new.append(syntax_sugar(val))#recurse
        else:
            new.append(val)
        index += 1
    return recursive_mll(new)
            
            
def make_linked_list(iterable):
    """Makes a linked list out of an iterable."""
    if len(iterable) == 0:
        return None #EMPTY LIST == Nil not (Nil, Nil)
    current = Cons(None, None)
    start = current #remember the start of the linked list (works because cons are mutable)
    for i in iterable:
        if current.car == None:
            current.car = i
        else:
            current.cdr = Cons(i, None)
            current = current.cdr
    return start

def recursive_mll(iterable):
    """Makes a linked list out of an iterable. Any iterables inside the 
    iterable are make into linked lists too, and so on."""
    new = []
    for i in iterable:
        if type(i) != list: 
            new.append(i)
        else:
            new.append(recursive_mll(i))
    return make_linked_list(new)

def iterate(linked): #makes an iterable out of a linked list. needed because linked lists are just Cons
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

#NOT CURRENTLY USED, BUT COULD COME IN HANDY

#def append_to_linkedlist(linked, obj):
#    current = linked
#    while current.cdr != None:
#        current = current.cdr
#    current.cdr = Cons(obj, None)
#    return current

def primitive(func):
    return (type(func) in (FunctionType, BuiltinFunctionType))

def tokenise(code_str):
    tokenised = []
    token_built = ""
    string_mode = False
    code_iter = iter(code_str)
    for i in code_iter:
        if string_mode:
            if i == '"':
                string_mode = False
                continue
            elif i == "\\":
                token_built += next(code_iter)
                continue
            else:
                token_built += i
                continue
        if i == "\\":
            token_built = Symbol(token_built + Symbol(next(code_iter)))
            continue
        elif i == '"':
            string_mode = True
            continue
        elif i == "(":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Token("(")) #tokens are represented by themselves for now
        elif i == ")":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Token(")"))
        elif i == " " or i == "\n":
            if (token_built != ""): tokenised.append(token_built) 
            token_built = ""
        elif i == "'":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Token("'"))
        else:
            token_built = Symbol(token_built + Symbol(i))
    return tokenised

   
def parse(tokenised):
    """Turns tokens into objects, parses parens."""
    #I forgot how this works and why I named this listy.
    #I was sleep-deprived, okay?
    listy = [[]]
    for token in tokenised:
        if token == Token("("):
            listy.append([])
        elif token == Token(")"):
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


def apply(function, environment):
    """Applies a function in an environment."""
    func = function.car
    vals = function.cdr
    try:
        first = function.cdr.car
    except:
        pass #0arg func
    try:
        second = function.cdr.cdr.car
    except:
        pass #1arg func
    if func == Symbol('cond'):
        for cons in iterate(vals):
            if evaluate(cons.car, environment) != None:    #None is the only false value; it is also the empty list.
                return evaluate(cons.cdr.car, environment) 
        raise Exception('no default in cond, and it fell through!!') #should this be the behaviour? discuss.
    
    elif func == Symbol('quote'):
        return first
    
    elif func == Symbol('vau'):
        return Vunction(function, environment)
    
    elif func == Symbol('define'):
        environment[first] = evaluate(second, environment)
        return environment[first]
    
    elif func == Symbol('set!'):
        environment.innermost(first)[first] = evaluate(second, environment)
        return environment.innermost(first)[first]
    
    elif func == Symbol('current-env'):
        return copy.deepcopy(localvars)#here the mutable nature of dicts works against us, requiring a copy
    
    elif func == Symbol('eval'):
        expr = evaluate(first, environment)
        env = evaluate(second, environment)
        return evaluate(expr, env)
    elif func == Symbol('load'):
        execute(syntax_sugar(parse(tokenise(open(first).read()))))
        return None
    else: #defined
        func = evaluate(func, environment)
        #vals not evaluated; vau
        
        if primitive(func):
            if 'operative' in dir(func):      #little bit of a hack to allow use
                return func(environment, vals)#of builtin functions. its ok tho
            
            else: #these are the primitive functions.
                evaluateds = [evaluate(i, environment) for i in iterate(vals)]
                return func(*evaluateds)
            
        return func(environment, vals) 

def evaluate(expr, environment):
    """Evaluates an expression in an environment."""
    if type(expr) == Cons:
        return apply(expr, environment)
    else:
        if type(expr) == Symbol:
            return environment.innermost(expr)[expr]#variables!
        else:
            return expr

def defaultenv():
    """Creates a sane environment."""
    env = Environment({}, parent=None)
    def wrap(environment, args):
        func = args.car
        func = evaluate(func, environment)
        def newfunc(*vals):
            evaluateds = make_linked_list(
                    [evaluate(i, environment) for i in (vals)])
            if primitive(func) and 'operative' not in dir(func):
                return func(*evaluateds)
            else:
                return func(environment, evaluateds)
        return newfunc

    wrap.operative = True

    #NORMAL FUNCTIONS
    
    def eq(val0, val1):                               #MCCARTHY PAPER VERSION
        if type(val0) == Cons or type(val1) == Cons:
            raise Exception("eq doesn't take a cons") #why not? i don't know, but this is helpful to me
        return Symbol("t") if val0 == val1 else None

    def begin(*vals):
        return vals[-1]
    
    def lst(*vals):
        return make_linked_list(vals)
    
    def set_parent(environment, parentenv):
        environment.parent, environment[1].parent = parentenv
        return environment
    
    def modify_parent(environment, parentenv):
        newenv = copy.deepcopy(environment)
        newenv.parent, newenv[1].parent = parentenv
        return newenv
    
    def set_env(environment, a, b):
        environment.innermost(a)[a] = b
        return environment
    
    def modify_env(environment, a, b):
        newenv = copy.deepcopy(environment)
        newenv.innermost(a)[a] = b
        return newenv
    
    def define_env(environment, a, b):
        environment[a] = b
        return environment
    
    def defmodify_env(environment, a, b):
        newenv = copy.deepcopy(environment)
        newenv[a] = b
        return environment
    
    def in_envp(environment, a):
        try:
            environment[0].innermost(a)
            return Symbol("t")
        except:
            return None
        
    def in_top_envp(environment, a):
        try:
            environment[0][a]
            return Symbol("t")
        except:
            return None


        
    env.update([(Symbol(i[0]),i[1]) for i in
    [
        ('wrap', wrap),
        
        ('eq?', eq), #('eq?', (lambda x,y: x is y), #SCHEME VERSION
                    
        ('car', (lambda x: x.car)),
                    
        ('cdr', (lambda x: x.cdr)),
                    
        ('cons', (lambda x, y: Cons(x, y))),
                    
        ('atom?', (lambda x: Symbol('t') if (not isinstance(x, Cons)) else None)),

        ('pair?', (lambda x: Symbol('t') if isinstance(x, Cons) else None)),

        ('combiner?', (lambda x: Symbol("t") if callable(x) else None)),
                    
        ('begin', begin),
                    
        ('exit', (lambda x=None: exit() if x == None else exit(x))),
                    
        ('write', (lambda x: print(x) or x)), #dirty hack -- print always returns None :. this always returns x, but prints x first.

        #####           
        #ENV#
        #####      
        ('environment?', (lambda x: Symbol("t") if isinstance(x, Environment) else None)),
                    
        ('set-parent!', set_parent),
                    
        ('modify-parent', modify_parent),
                    
        ('parent', (lambda x: (x[0].parent, x[1].parent))),
                    
        ('parent?', (lambda x, y: Symbol("t") if ((x[0].parent is y[0]) and (x[1].parent is y[1])) else None)),
                    
        ('set-in-env!', set_env),
                    
        ('modify-in-env', modify_env),
                    
        ('define-in-env!', define_env),
                    
        ('modify-define-in-env', defmodify_env),
                    
        ('in-env', (lambda x, y: x.innermost(y)[y])),
                    
        ('in-env?', in_envp),
                    
        ('in-top-env?', in_top_envp),

        #######            
        #MATHS#
        #######           
        ('+', (lambda x, y: x+y)),
                    
        ('-', (lambda x, y: x-y)),
                    
        ('*', (lambda x, y: x*y)),
                    
        ('/', (lambda x, y: x/y)),

        #######            
        #LOGIC#
        #######            
        ('not', (lambda x: Symbol("t") if x == None else None)),

        #######
        #MISC.#
        #######
        ('number?', (lambda x: type(x) == int or type(x) == float)),

        #######
        #LISTS#
        #######
        ('list', lst)
    ]])
    return env

defaultenv = defaultenv()
def execute(parsed, env):          #executes program
    """Executes program."""
    for i in iterate(parsed):
        evaluate(i, env)    #run each lisp.

def run(program):
    execute(syntax_sugar(parse(tokenise(program))),defaultenv)

if __name__ == "__main__": #auto-test
    test ="""
(write "basic test")
(write ((vau (x y z) env (eval x env)) 1 2 3))
(write ((wrap (vau (x) env x)) wrap))
(define x 1)
(write x)

"""
    run(test)
