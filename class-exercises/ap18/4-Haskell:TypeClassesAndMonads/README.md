<body>
<div id="content">
<h1 class="title">Haskell: Type Classes and Monads</h1>

<div id="outline-container-org64807a1" class="outline-2">
<h2 id="org64807a1">Introduction</h2>
<div class="outline-text-2" id="text-org64807a1">
<p>
All the exercises below consider (variants of) the following ADT for simple expressions:
</p>
<pre class="example">data Expr a = Const a | Sum (Expr a) (Expr a) | Mul (Expr a) (Expr a)
</pre>
</div>
</div>


<div id="outline-container-orgd77499e" class="outline-2">
<h2 id="orgd77499e">Exercise 1</h2>
<div class="outline-text-2" id="text-orgd77499e">
<p>
   Define a recursive evaluation function <code>eval</code> for expressions. Test the function on 
a couple of simple expressions. For example, 
</p>
<div class="_EXAMPLE">
<p>
eval (Sum (Mul (Const 2) (Const 3)) (Const 4))
</p>

</div>
<p>
should evaluate to <code>10</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Warming up!</li>
<li><b>Expected output:</b> A function <code>eval</code> that recursively evaluates an expression.</li>
</ul>
</div>
</div>

<div id="outline-container-orgaa09f9a" class="outline-2">
<h2 id="orgaa09f9a">Exercise 2</h2>
<div class="outline-text-2" id="text-orgaa09f9a">
<p>
    Enrich the above expressions with a new constructor <code>Div (Expr a) (Expr a)</code> and write an evaluation function <code>safeEval</code> 
for these extended expressions, interpreting <code>Div</code> as integer division. Test the new function with some expressions.
</p>

<p>
    <b>Hint:</b> Function <code>safeEval</code> must be partial, since
division by zero is undefined, and thus it must return a <code>Maybe</code> value.
</p>

<ul class="org-ul">
<li><b>Goal:</b> First steps with partial functions.</li>
<li><b>Expected output:</b> A function <code>safeEval</code> that recursively evaluates extended integer expressions.</li>
</ul>
</div>
</div>

<div id="outline-container-org17656b0" class="outline-2">
<h2 id="org17656b0">Exercise 3</h2>
<div class="outline-text-2" id="text-org17656b0">
<p>
Define an instance of the constructor class <code>Functor</code> for the expressions of <a href="#orgd77499e">Exercise 1</a>, in order to be able to <code>fmap</code> over trees.
A call to <code>fmap f e</code>  (where <code>e :: Expr a</code> and <code>f :: a -&gt; b</code>) should return an expression of type <code>Expr b</code> 
obtained by replacing all the <code>Const v</code> nodes in <code>e</code> with <code>Const (f v)</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experimenting with constructor classes.</li>
<li><b>Expected output:</b> An instance <code>Functor Expr</code>, as requested.</li>
</ul>
</div>
</div>

<div id="outline-container-org8336c3d" class="outline-2">
<h2 id="org8336c3d">Exercise 4</h2>
<div class="outline-text-2" id="text-org8336c3d">
<p>
    Propose a way to define an instance <code>Foldable Expr</code> of the class constructor <code>Foldable</code>, by providing a 
function to fold values across a tree representing an expression.
</p>

<p>
<b>Hint:</b> Consult <a href="https://www.haskell.org/hoogle">Hoogλe</a> to discover the "Minimal complete definition" of <code>Foldable</code>. Several solutions are possible.  
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experimenting with the <code>Foldable</code> constructor class, and understanding Haskell documentation.</li>
<li><b>Expected output:</b> An instance <code>Foldable Expr</code>, as requested.</li>
</ul>
</div>
</div>
<div id="outline-container-orgb254c9c" class="outline-2">
<h2 id="orgb254c9c">Exercise 5</h2>
<div class="outline-text-2" id="text-orgb254c9c">
<p>
Consider the following definition of variables for the expressions of <a href="#orgd77499e">Exercise 1</a>:
</p>
<pre class="example">data Var = X | Y | Z
data Expr a = ... | Id Var
</pre>

<p>
First, define a function <code>subst</code> that takes a triple <code>(x, y, z)</code> of expressions, interpreted as the values of <code>X</code>, <code>Y</code>, <code>Z</code>
 
respectively, and an expression and produces a new expression where the 
variables are substituted with the corresponding expressions.
</p>

<p>
    Next define functions <code>eval</code> and <code>recEval</code>. The <b>partial</b> function <code>eval</code>, applied to an expression <code>e</code>, returns its value if 
<code>e</code> does not contain variables, and <code>Nothing</code> otherwise. 
Function <code>recEval</code> takes as arguments a triple of expressions <code>(x, y, z)</code> and an expression <code>e</code>, and evaluates <code>e</code> replacing variables 
with the corresponding expressions when needed.
</p>

<p>
Finally, compare the effect of applying function <code>recEval</code> and <code>subst.eval</code> to a triple of  expressions <code>(x, y, z)</code> and an expression
<code>e</code>. Do they always deliver the same result?
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experiment a little more with partial function.</li>
<li><b>Expected output:</b> An implementation of the <code>subst</code>, <code>eval</code> and <code>recEval</code>  functions.</li>
</ul>
</div>
</div>

<div id="outline-container-org62480d0" class="outline-2">
<h2 id="org62480d0">Exercise 6</h2>
<div class="outline-text-2" id="text-org62480d0">
<p>
Write an instance of <code>Show</code> that allows to print expressions  (with parenthesis!).
</p>

<p>
<b>Hint:</b> Take a look at the <a href="http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:show">doc of Show</a>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Giving another try to type classes.</li>
<li><b>Expected output:</b> An instance of <code>Show</code> of <code>Expr</code>.</li>
</ul>
</div>
</div>

<div id="outline-container-org2c7973d" class="outline-2">
<h2 id="org2c7973d">Exercise 7</h2>
<div class="outline-text-2" id="text-org2c7973d">
<p>
Consider the <code>eval</code> function of <a href="#orgd77499e">Exercise 1</a>.  Exploiting the IO monad, Write two new versions of <code>eval</code>:
</p>
<ol class="org-ol">
<li><code>evalPrint</code>, that directly prints the final result of the expression under evaluation</li>
<li><code>evalPrintSub</code>, that also prints all the intermediate results</li>
</ol>

<p>
<b>Hint:</b> Take a look at the <a href="http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:IO">IO Monad</a> documentation.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experimenting with the I/O monad.</li>
<li><b>Expected output:</b> The two requested functions.</li>
</ul>
</div>
</div>
</div>