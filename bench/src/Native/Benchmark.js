Elm.Native.Benchmark = {};
Elm.Native.Benchmark.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Benchmark = localRuntime.Native.Benchmark || {};
	if (localRuntime.Native.Benchmark.values)
	{
		return localRuntime.Native.Benchmark.values;
	}

	var List = Elm.List.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);


	// RUN SOME BENCHMARKS

	function run(benchmark)
	{
		return Task.asyncFunction(function(callback)
		{
			var overallSuite = new Benchmark.Suite;
			var statistics = addTests(overallSuite, benchmark);
			overallSuite.on('complete', function(event) {
				callback(Task.succeed(statistics));
			});
			overallSuite.run({ async: true });
		});
	}


	// ADD TESTS TO THE JS BENCHMARK SUITE

	function addTests(overallSuite, benchmark)
	{
		if (benchmark.ctor === 'Suite')
		{
			return {
				ctor: 'StatSuite',
				_0: benchmark._0,
				_1: A2(List.map, curriedAddTests(overallSuite), benchmark._1)
			};
		}

		// WE MUST BE LOOKING AT A TEST
		var name = benchmark._0;
		var thunk = benchmark._1;

		var statistics = {
			ctor: 'Result',
			_0: name,
			_1: null
		};

		overallSuite.add(name, thunk, {
			onStart: function() {
				console.log('running: ' + name);
			},
			onComplete: function(event) {
				var jsBenchmark = event.target;
				statistics._1 = {
					mean: jsBenchmark.stats.mean,
					hz: jsBenchmark.hz
				};
			}
		});

		return statistics;
	}

	var curriedAddTests = F2(addTests);



	return localRuntime.Native.Benchmark.values = {
		makeThunk: function(x) { return x; },
		run: run
	};
};
