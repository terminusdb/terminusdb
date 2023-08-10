'use strict'

const settings = {
  recursive: true,
  timeout: '900000',
  parallel: process.env.MOCHA_PARALLEL === 'true',
}

if (typeof process.env.MOCHA_JOBS !== 'undefined') {
  settings.jobs = parseInt(process.env.MOCHA_JOBS)
}

module.exports = settings
