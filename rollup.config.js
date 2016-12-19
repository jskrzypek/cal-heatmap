import node from 'rollup-plugin-node-resolve';
import uglify from 'rollup-plugin-uglify';

const min = process.env.MIN || false;

export default {
  entry: 'src/cal-heatmap.js',
  exports: 'default',
  dest: min ? 'cal-heatmap.umd.min.js' : 'cal-heatmap.umd.js',
  format: 'umd',
  sourceMap: true,
  moduleName: 'CalHeatMap',
  external: ['d3'],
  globals: { 'd3': 'd3' },
  plugins: min ? [node(), uglify()] : [node()]
};