<body>
<div id="content">
<h1 class="title">Java 8 Features</h1>

<div id="outline-container-sec-1" class="outline-2">
<h2 id="sec-1">Instructions</h2>
<div class="outline-text-2" id="text-1">
<p>
The following exercises require using the files
<code>oscar_age_female.csv</code> and <code>oscar_age_male.csv</code> that store
information about the winners of the Oscar prize. These are text
files where each line is a record storing the following fields in
csv format:
</p>
<pre class="example">Index, Year, Age, Name, Movie
</pre>
<p>
where the field <i>Year</i> denotes the year when the Oscar was won, and
<i>Age</i> the age the winners had when they were awarded. The other
fields are self-explanatory.
</p>

<p>
<b>Solution format</b>: implement all the required methods below in a
file <code>WinnerOpsDB.java</code>, adequately commented.
</p>
</div>
</div>

<div id="outline-container-sec-2" class="outline-2">
<h2 id="sec-2">Exercise 1</h2>
<div class="outline-text-2" id="text-2">
<p>
Write your own class that implements the provided interface <code>Winner</code>
that represents a record of the winner database. Then, write
a static method <code>loadData</code> that given a <code>String[]</code> containing the paths of winners
databases returns a <code>Stream&lt;Winner&gt;</code> representing the
content of the databases. Note that the first line of each file
describes the format of the fields and that the values of fields <code>Name</code>
and <code>Movie</code> are enclosed in double quotes, e.g., "Coquette". Your
solution must opportunely deal with these cases.
</p>
</div>
</div>


<div id="outline-container-sec-3" class="outline-2">
<h2 id="sec-3">Exercise 2</h2>
<div class="outline-text-2" id="text-3">
<p>
Implement a static method <code>youngWinners</code> that given a
<code>Stream&lt;Winner&gt;</code> returns a new <code>Stream&lt;Winner&gt;</code> containing the
winners that are <i>younger</i> than 35 <i>ordered alphabetically by
names</i>.
</p>
</div>
</div>

<div id="outline-container-sec-4" class="outline-2">
<h2 id="sec-4">Exercise 3</h2>
<div class="outline-text-2" id="text-4">
<p>
Write a static method <code>extremeWinners</code> that given a <code>Stream&lt;Winner&gt;</code>
returns a <code>Stream&lt;Winner&gt;</code> containing extractly two winners, one
among the youngests and one among the oldests, <i>ordered
alphabetically by names</i>.
</p>
</div>
</div>



<div id="outline-container-sec-5" class="outline-2">
<h2 id="sec-5">Exercise 4</h2>
<div class="outline-text-2" id="text-5">
<p>
Write a static method <code>multiAwardedPerson</code> that given a
<code>Stream&lt;Winner&gt;</code> returns a <code>Stream&lt;String&gt;</code> containing the names of
winners that won a prize at least twice <i>ordered alphabetically by
names</i>.
</p>
</div>
</div>


<div id="outline-container-sec-6" class="outline-2">
<h2 id="sec-6">Exercise 5</h2>
<div class="outline-text-2" id="text-6">
<p>
  Write a static method <code>multiAwardedFilm</code> that given a
  <code>Stream&lt;Winner&gt;</code> returns a <code>Stream&lt;String&gt;</code> containing the title of
  films who won two prizes. The elements of the stream must be in
  <i>chronological order</i>, i.e. in increasing order of year of the corresponding 
records in the database.
</p>
</div>
</div>


<div id="outline-container-sec-7" class="outline-2">
<h2 id="sec-7">Exercise 6</h2>
<div class="outline-text-2" id="text-7">
<p>
Write a static method <code>measure</code> that given a
<code>Function&lt;Stream&lt;T&gt;,Stream&lt;U&gt;&gt; f</code> and a <code>Stream&lt;T&gt; s1</code> returns a 
<code>long</code> denoting the time
in nanosecs required to invoke  <code>s2.collect(Collectors.toList())</code>, where
<code>s2</code> is the stream returned by <code>f.apply(s1)</code>. 
Furthermore, write another static
method <code>runJobs</code> that given a
<code>Stream&lt;Function&lt;Stream&lt;T&gt;,Stream&lt;U&gt;&gt;&gt;</code> of jobs and a <code>Stream&lt;T&gt; s</code> returns a
<code>LongStream</code> of the times required to run each job by invoking <code>measure</code>. Then, provide
parallel versions of the methods developed for Exercises 2 to 5
(their names must end the <code>Parallel</code> suffix, e.g,
<code>multiAwardedFilmParallel</code>). Finally, implement a method
<code>comparison</code> that given a <code>Stream&lt;Winner&gt;</code> returns an array of 11 <code>long</code> values as follows:
elements with indexes in [2,5] report the time required to run the
sequential version of Exercises 2 to 5; whereas elements with
indexes [7,10] report the time to run the corresponding parallel
versions. 
</p>

<p>
<b>Note</b>: You can decide whether to use method <code>runJobs</code> or not, when writing
method <code>comparison</code>. In both cases justify your choice.
</p>

<p>
<b>Hint</b>: Consider the method <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#nanoTime--">System.nanoTime</a> to measure the execution
time.
</p>
</div>
</div>
</div>