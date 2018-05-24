flags = [
'-Wall',
#'-stdlib=libc++',
'-x', 'c++',
'-std=c++11',
'-isystem',
'/usr/include/c++/7',
'-isystem',
'/usr/include/x86_64-linux-gnu/c++/7']

def FlagsForFile( filename, **kwargs ):
  return {
    'flags': flags
  }
