#! /usr/bin/env python
# encoding: utf-8

import Params,subprocess,boost,sys

VERSION='0.0.1'
APPNAME='plap'
srcdir='.'
blddir='build'
opt_levels={'coverage' : ['-fprofile-arcs -ftest-coverage',
			  '-fprofile-arcs -ftest-coverage'],
	    'default'  : ['',''],
	    'normal'   : ['-O2','-O2'],
	    'ultra'    : ['-O2 -DNDEBUG -DNLOGGING','-O2']}
src='''
lang/builtin.cc
lang/context.cc
lang/core.cc
lang/func.cc
lang/vertex.cc
lang_io/analyze.cc
lang_io/names.cc
lang_io/operators.cc
lang_io/parse.cc
lang_io/pretty_print.cc
lang_io/repl.cc
util/io.cc
util/tree_io.cc
'''
includes='lang lang_io test util'

#Params.reset_colors() #if you don't like colors, uncomment

def set_options(opt):
	opt.add_option('-O','--opt-level',action ='store',default='default',
		       help='set optimization level to: coverage (compute'+
		       'code coverage), default (none), '+
		       'normal (O2), or ultra (O2, assertions and logging '+
		       'disabled)',dest='opt_level')
	opt.add_option('--profile',action='store_true',default=False, 
		       help='link to perftools cpu profiler')
	opt.add_option('--test',action='store_true',default=False, 
		       help='just build the unit tests, then run them')
	opt.add_option('--repl',action='store_true',default=False, 
		       help='just build the read-eval-print loop, then run it')
	opt.tool_options('compiler_cxx')

def configure(conf):
	conf.check_tool('compiler_cxx')
	conf.env['WANT_BOOST_MIN']='1.34.1'
	boost.detect_boost(conf)

	conf.env['LIB_PROFILER']='profiler'

	def opt_set(name,flags):
		conf.env['CXXFLAGS_OPT_'+name.upper()]=flags[0]
		conf.env['LINKFLAGS_OPT_'+name.upper()]=flags[1]

	for name in opt_levels:
		opt_set(name,opt_levels[name])

def build(bld):
	if Params.g_options.opt_level not in opt_levels:
		Params.error('unrecognized optimization level '+
			     Params.g_options.opt_level)
		exit(1)

	#everything gets put in a simple static library (plaplib)
	obj=bld.create_obj('cpp', 'staticlib')
	obj.source=src
	obj.includes=includes
	obj.target='plaplib'
	
	#executables are just a main which gets linked with plaplib
	def build_program(name):
		obj=bld.create_obj('cpp','program')
		obj.source='main/'+name+'.cc'
		obj.includes=includes
		obj.target=name
		obj.uselib='OPT_'+Params.g_options.opt_level.upper()
		obj.uselib_local='plaplib'
		if Params.g_options.profile:
			obj.uselib=obj.uselib+' PROFILER'
	
	if Params.g_options.test or not Params.g_options.repl:
		build_program('test_runner') #unit tests
	if Params.g_options.test:
		return #don't need to build anything else
	build_program('repl') #read-eval-print loop
	if Params.g_options.repl:
		return

def shutdown():
	if Params.g_options.test: 	#option to run the unit tests
		if Params.g_verbose:
			v="--log_level=all"
		else:
			v="--report_level=short"
		subprocess.call(["build/default/test_runner",v])
		if Params.g_options.opt_level=='coverage':
			cmd="gcov test_runner -o default/main"
			cmd+=" | grep 'File.*\.\./' -C 1"
			subprocess.call(cmd,shell=True,cwd='build')
	if Params.g_options.repl: 	#option to run the read-eval-print loop
		subprocess.call(["build/default/repl"])
