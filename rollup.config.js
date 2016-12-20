import node from 'rollup-plugin-node-resolve';
import conditional from 'rollup-plugin-conditional';
import uglify from 'rollup-plugin-uglify';

const min = process.env.MIN || false;
const target = process.env.TARGET || 'umd';
const full = process.env.FULL || false;

let destFileSuffix = target === 'es' ? 'es2015' : target;
if (full) {
  destFileSuffix += '.full'; 
} 
if (min) {
  destFileSuffix += '.min';
} 

export default {
  entry: 'src/cal-heatmap.js',
  exports: 'default',
  dest: `cal-heatmap.${destFileSuffix}.js`,
  format: target,
  sourceMap: true,
  moduleName: 'CalHeatMap',
  external: full ? [] : [
    'd3',
    'd3-array',
    'd3-collection',
    'd3-format',
    'd3-interpolate',
    'd3-request',
    'd3-scale',
    'd3-selection',
    'd3-time',
    'd3-time-format'
  ],
  globals: full ? {} : { 
    'd3': 'd3',
    'd3-array': 'd3',
    'd3-collection': 'd3',
    'd3-format': 'd3',
    'd3-interpolate': 'd3',
    'd3-request': 'd3',
    'd3-scale': 'd3',
    'd3-selection': 'd3',
    'd3-time': 'd3',
    'd3-time-format': 'd3'
  },
  plugins: [node(), conditional({ condition: min, plugin: uglify() })]
};