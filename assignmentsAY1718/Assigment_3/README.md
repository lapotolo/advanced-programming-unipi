<body>

<div id="preamble">

</div>

<div id="content">
<h1 class="title">Refactoring in Python</h1>


<div id="outline-container-1" class="outline-2">
<h2 id="sec-1">Instructions</h2>
<div class="outline-text-2" id="text-1">

<p>  The assignment requires that you use functions, types and classes from
  the <a href="https://docs.python.org/3/library/index.html">Python standard library</a>. 
</p>
<p>
  <b>Solution format</b>: implement the required script in a file
  <code>refactor.py</code>, adequately commented.
</p>
</div>

</div>

<div id="outline-container-2" class="outline-2">
<h2 id="sec-2">Script Specifications</h2>
<div class="outline-text-2" id="text-2">


<p>
  Write a Python script <code>refactor.py</code> that takes two arguments: a URL
  and a path. The URL must point to a Java source file on a <i>remote</i>
  host; whereas the second argument must be the path of a <i>local</i> file
  containing a Java snippet. This snippet can be assumed to be the
  declaration of a single <code>static</code> method that takes an <code>Object</code> as
  argument and returns a <code>String</code>.  Note that you should <b>not</b> make
  any assumption on the method name, i.e. methods in different snipped files
  can have different names.
</p>
<p>
  Your script must do the following:
</p><ol>
<li>download the <i>remote</i> file: you can assume that the file contains the 
     definition of a single class; 
</li>
<li>copy the snippet inside the class defined in the <i>remote</i> file;
</li>
<li>perform a refactoring of all the invocations of
     <code>System.out.println</code> with an argument, by wrapping such argument
     with a call to the  method copied in the previous point. For
     example, suppose that the snippet method is called <code>capitalize</code>
     and consider the statement



<pre class="example">System.out.println("the result of math.sqrt(2) is " + Math.sqrt(2.0));
</pre>

<p>
     your refactoring should result in the statement
</p>


<pre class="example">System.out.println(capitalize("the result of math.sqrt(2) is " + Math.sqrt(2.0)));
</pre>

</li>
<li value="4">compile the refactored file; if the compilation fails report an
     error message to the user, and redirect the compiler messages to the
     file <code>COMP_ERR.txt</code> before terminating (this file must be create only if compilation
     errors occur);
</li>
<li>if the compilation succeeds, run the resulting Java class and save the
     output of the execution in the file <code>OUTPUT.txt</code>, and possible
     error messages in the file <code>ERROR.txt</code>, then terminate.
</li>
</ol>


<p>
  You can test your script using the snippet files <code>snip1.java</code> and
  <code>snip2.java</code> (available on the course webpage). However, the code of
  your script should be parametric and should not make any assumption
  on the names of the Java file and of the method in the snippet file.
</p>
<p>
  Finally, your script <b>must</b> handle anomalous situations, e.g. when
  the script is invoked with less arguments than expected; or when the
  URL is invalid; or the needed files do not exist.
</p>
<p>
  <b>Hint</b>: Here are some Java files, you can use to test your script:
</p><ul>
<li><a href="http://pages.di.unipi.it/corradini/Didattica/AP-17/PROG-ASS/03/ClassWithTest.java">ClassWithTest.java</a> 
</li>
<li><a href="https://gist.githubusercontent.com/StrixG/bb41fc980a819427bb20/raw/2827318dbb1369fce61d626e36ff38a74fcf7cae/Main.java">Main.java</a>
</li>
<li><a href="https://gist.githubusercontent.com/UmperiaFriend/3258ab2777127d2ca6995534fc2bc928/raw/0aff444fb0b210f6074c4e7d2d2c6273202cc7d4/Client.java">Client.java</a>
</li>
<li><a href="https://gist.githubusercontent.com/dsahapersonal/3c90515040bb511f4e5d1d817a69a90f/raw/2cbd571d2bccea1f836f6d3e7fb3d6a523a9a30e/insertion_Sort.java">insertion_Sort.java</a>
</li>
</ul>


</div>
</div>
</div>