Elm.Native.Benchmark = {};
Elm.Native.Benchmark.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Benchmark = localRuntime.Native.Benchmark || {};
	if (localRuntime.Native.Benchmark.values)
	{
		return localRuntime.Native.Benchmark.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);
        var Signal = Elm.Signal.make(localRuntime);
        var Utils = Elm.Native.Utils.make(localRuntime);

        //Nothing special happens here, but we need to use it as a Native function
        //So that we can accept thunks of any type
        function makeBenchmark(name, thunk)
        {
            return {name : name, thunk : thunk};
        }

        //Generate the task for running a benchmark suite
        //Possibly updating a given mailbox with a string describing
        //Our progress of running the benchmarks so far
	function runWithProgress(maybeTaskFn, inSuite)
	{
            
	    return Task.asyncFunction(function(callback) {
                var bjsSuite = new Benchmark.Suite;
                var benchArray;
                var retData = [];
                var finalString = "";
                var numCompleted = 0;
                var numToRun;
                
                switch (inSuite.ctor)
                {
                    case "Suite":
                      benchArray = List.toArray(inSuite._1);
                      break;
                    case "SingleBenchmark":
                      benchArray = [inSuite._0 ];
                      break;
                }

                numToRun = benchArray.length;
                Task.perform(maybeTaskFn("Running benchmark 1 of " + numToRun));

                //Initialize each benchmark in the suite
                for (i = 0; i < benchArray.length; i++)
                {
                    var ourThunk = function (){
                        //Run the thing we're timing, then mark the asynch benchmark as done
                        benchArray[i].thunk();
                        deferred.resolve();
                        }
                    bjsSuite.add(benchArray[i].name, benchArray[i].thunk );
                }

                //Every time a benchmark finishes, we store its results
                //and update our progress string
                bjsSuite.on('cycle', function(event) {
                   numCompleted += 1;
                   retData.push(
                       { name : event.target.options.name
                       , hz : event.target.hz
                       , marginOfError : event.target.stats.moe
                       , moePercent : event.target.stats.rme
                       }
                       );
                   finalString += String(event.target) + "\n";
                   var intermedString = 
                        "Running benchmark " 
                        + (numCompleted + 1) 
                        + " of " + numToRun 
                        + "\nLast result: " + String(event.target);
                   Task.perform(maybeTaskFn(intermedString));
                   //retString += String(event.target) + "\n";
                });
                
                //When the last benchmark finishes, we show all results collected
                bjsSuite.on('complete', function(event) {
                   Task.perform(maybeTaskFn("Final results:\n\n" + finalString) );
                   return callback(Task.succeed(Utils.Tuple2(finalString, List.fromArray(retData))));
                });

                //Finally: actually run the suite
                Task.perform(
                  Task.asyncFunction(function(otherCallback){
                      bjsSuite.run({ 'async': true });
                  }));
        });
             }

	return localRuntime.Native.Benchmark.values = {
		makeBenchmark: F2(makeBenchmark),
		runWithProgress: F2(runWithProgress)
	};
                
   
};
