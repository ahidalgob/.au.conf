flags = [
'-Wall',
#'-stdlib=libc++',
'-std=c++11',
'-x', 'c++']

def FlagsForFile( filename, **kwargs ):
  return {
    'flags': flags
  }
