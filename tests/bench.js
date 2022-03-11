const glob = require('fast-glob')
const path = require('path')
const process = require('process')
const { default: Benchmark } = require('buffalo-bench')

const { util } = require('./lib')

const globOptions = {
  onlyFiles: true,
  braceExpansion: false,
  extglob: false,
}

const allFilePattern = './bench/**/*.js'
const defaultMinSamples = 5

async function main () {
  // Remove `node` and this file name from the arguments.
  const args = process.argv.slice(2).filter(x => x !== '--json')
  const outputJson = process.argv.includes('--json')

  // If there are file pattern arguments, use them. Otherwise, use a pattern for
  // all of the benchmark files.
  const filePatterns = args.length > 0 ? args : [allFilePattern]

  // Get an asynchronous stream of file paths from the file patterns.
  const filePaths = glob.stream(filePatterns, globOptions)

  // Use this to verify that at least one benchmark was run.
  let iteration = 0
  const benches = []
  // Iterate over each file path.
  for await (const filePathInput of filePaths) {
    // Normalize relative and non-relative input paths.
    const filePath = path.relative(process.cwd(), filePathInput)
    // Skip templates
    if (filePath.match(/\.template\.js$/)) {
      continue
    } else {
      iteration++
    }
    // Load the benchmark options from the file. Without the './', require
    // thinks it's a module name.
    const options = require('./' + filePath)
    if (util.isNonEmptyObject(options)) {
      throw new Error(
        `Error! Unexpected value loaded from '${filePath}': ${JSON.stringify(options)}`,
      )
    }
    // Set the default `minSamples`: `buffalo-bench` default (1) is too small.
    if (util.isUndefinedOrNull(options.minSamples)) {
      options.minSamples = defaultMinSamples
    }
    if (!outputJson) {
      console.log(`>>> Running ${filePath} (>= ${options.minSamples} samples)`)
    }
    // Create and run the benchmark.
    const bench = new Benchmark(filePath, options)
    await bench.run()
    // Throw an error if one occurred.
    if (bench.error) {
      throw bench.error
    }

    // Report the results or push it to list of results
    if (outputJson) {
      benches.push(bench.toJSON())
    } else {
      console.log(bench.toJSON())
      console.log('>>> Completed', filePath)
      console.log()
    }
  }
  if (iteration === 0) {
    throw new Error(`Error! No benchmarks found at these paths: ${filePatterns}`)
  }
  if (outputJson) {
    console.log(JSON.stringify(benches))
  }
}

main().catch((err) => {
  // Report any error and exit with a non-zero exit code.
  console.error(err)
  process.exit(1)
})
