const sass = require('node-sass');

module.exports = function (grunt) {
  grunt.initConfig({
    sass: {
      options: { implementation: sass },
      dist: {
        options: { style: 'compressed', sourcemap: 'none' },
        files: { 'public/css/docs.css': 'scss/docs.scss' }
      }
    },
    autoprefixer: {
      dist: {
        files: {
          'public/css/docs.css': 'public/css/docs.css'
        }
      }
    },
    watch: {
      styles: {
        files: ['scss/*.scss'],
        tasks: ['sass', 'autoprefixer']
      }
    }
  });
  grunt.loadNpmTasks('grunt-sass');
  grunt.loadNpmTasks('grunt-autoprefixer');
  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('default', ['sass', 'autoprefixer']);
};
