<body>
<div id="content">
<h1 class="title">Haskell &amp; Functional Programming</h1>

<div id="outline-container-org7c7ec13" class="outline-2">
<h2 id="org7c7ec13">Exercise 1</h2>
<div class="outline-text-2" id="text-org7c7ec13">
<p>
Write a function <code>myReplicate</code> that given an integer <code>n</code> and a value
<code>v</code> returns a list of <i>length</i> <code>n</code> initialized with <code>v</code>, namely all
elements are equal to <code>v</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Warming up!</li>
<li><b>Expected output:</b> Two implementations of <code>myReplicate</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org638890a" class="outline-2">
<h2 id="org638890a">Exercise 2</h2>
<div class="outline-text-2" id="text-org638890a">
<p>
Write a function <code>sumOdd</code> that given a list of integers computes the
sum of the values that are <i>odd</i>.
</p>

<p>
<b>Hint</b>: consider the functions <code>odd</code> and <code>even</code> of the <code>Prelude</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Warming up (pt. 2)!</li>
<li><b>Expected output:</b> Two implementations of <code>sumOdd</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org16bacc4" class="outline-2">
<h2 id="org16bacc4">Exercise 3</h2>
<div class="outline-text-2" id="text-org16bacc4">
<p>
Write a function <code>repl</code> that given a list <code>xs</code> and a integer <code>n</code>
returns a list containing the elements of <code>xs</code> replicated <code>n</code> times.
</p>

<p>
<b>Hint</b>: you can use the function <code>createList</code> of <a href="#org7c7ec13">Exercise 1</a>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Playing with lists.</li>
<li><b>Expected output:</b> Two implementations of <code>repl</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org1bb06f1" class="outline-2">
<h2 id="org1bb06f1">Exercise 4</h2>
<div class="outline-text-2" id="text-org1bb06f1">
<p>
Write a function <code>totalLength</code> that given a list of strings <code>xs</code>
computes the sum of the lengths of the strings starting with the
character '<code>A</code>'.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Test your skills with lists and strings.</li>
<li><b>Expected output:</b> Two implementations of <code>totalLength</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org18d8ac2" class="outline-2">
<h2 id="org18d8ac2">Exercise 5</h2>
<div class="outline-text-2" id="text-org18d8ac2">
<p>
Write a function <code>filterOdd</code> that given a list <code>xs</code> returns a new
list obtained from <code>xs</code> by removing the elements at odd positions.
</p>

<p>
<b>Hint</b>: Here "odd positions" means the first, third, fifth, etc
position.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Playing with lists (pt. 2).</li>
<li><b>Expected output:</b> Two implementations of <code>filterOdd</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-orge27e3a6" class="outline-2">
<h2 id="orge27e3a6">Exercise 6</h2>
<div class="outline-text-2" id="text-orge27e3a6">
<p>
Write a function <code>titlecase</code> that given a string <code>s</code> converts it to
<i>titlecase</i> by uppercasing the first letter of every word.
</p>

<p>
<b>Hint</b>: consider using the function <code>words</code>, <code>unwords</code> of the
<code>Prelude</code> and the function <code>toUpper</code> of the module <code>Data.Char</code>. To
make accessible this last function in your code use <code>import
    Data.Char (toUpper)</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experimenting with strings.</li>
<li><b>Expected output:</b> Two implementations of <code>titlecase</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-org1433865" class="outline-2">
<h2 id="org1433865">Exercise 7</h2>
<div class="outline-text-2" id="text-org1433865">
<p>
Write a function <code>countVowelPali</code> that given a list of strings <code>xs</code>
returns the total number of vowels in strings that are palindromes.
For example,
</p>
<pre class="example">countVowelPali ["anna", "banana", "civic", "mouse"] = 4
</pre>


<ul class="org-ul">
<li><b>Goal:</b> Fun with strings and lists (again :P).</li>
<li><b>Expected output:</b> Two implementations of <code>countVowelPali</code>: one recursive and one using the combinators <code>map</code>, <code>filter</code>, <code>foldl/r</code> from the <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html">Haskell Prelude</a>.</li>
</ul>
</div>
</div>

<div id="outline-container-orgc7646f0" class="outline-2">
<h2 id="orgc7646f0">Exercise 8</h2>
<div class="outline-text-2" id="text-orgc7646f0">
<p>
Recall the higher-order combinator <code>map</code> from the <code>Prelude</code>.
Implement it using the combinator <code>foldl</code>.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Experimenting with combinators.</li>
<li><b>Expected output:</b> A file containing the required implementation of the <code>map</code> combinator.</li>
</ul>
</div>
</div>

<div id="outline-container-orgb453661" class="outline-2">
<h2 id="orgb453661">Exercise 9</h2>
<div class="outline-text-2" id="text-orgb453661">
<p>
Consider the following definition of binary trees:
</p>
<pre class="example">data IntTree = Leaf Int | Node (Int, IntTree, IntTree)
</pre>

<ol class="org-ol">
<li>Implement <code>tmap</code>, a "tree version" of the <code>map</code> combinator. More precisely, the function <code>tmap</code> should take a function <code>f</code> and a tree <code>t</code> and should apply <code>f</code> to each value in <code>t</code>.</li>
<li>Using <code>tmap</code> implement the function <code>succTree</code> taking a tree <code>t</code> and computing a tree whose elements are the successors of the values in <code>t</code>.</li>
<li>Write a function <code>sumSucc</code> taking a tree <code>t</code> and computing the sum of the elements of <code>succTree t</code>.</li>
</ol>


<ul class="org-ul">
<li><b>Goal:</b> Experimenting with trees.</li>
<li><b>Expected output:</b> A file containing the three required functions.</li>
</ul>
</div>
</div>

<div id="outline-container-org4e0d534" class="outline-2">
<h2 id="org4e0d534">Exercise 10</h2>
<div class="outline-text-2" id="text-org4e0d534">
<p>
Implement a tail recursive version of the <code>map</code> and <code>filter</code> combinators.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Trying to write tail recursive functions.</li>
<li><b>Expected output:</b> A file containing the required combinators.</li>
</ul>
</div>
</div>

<div id="outline-container-org9a7e2b3" class="outline-2">
<h2 id="org9a7e2b3">Exercise 11</h2>
<div class="outline-text-2" id="text-org9a7e2b3">
<p>
Read the web page <a href="https://wiki.haskell.org/Foldr_Foldl_Foldl'">Foldr Foldl Foldl'</a>.
Write some minimal examples highlighting the differences between the three functions.
</p>

<ul class="org-ul">
<li><b>Goal:</b> Exploring alternative implementations of popular combinators.</li>
<li><b>Expected output:</b> A file containing the required examples.</li>
</ul>
</div>
</div>
</div>