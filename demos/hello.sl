# a "hello world" demo.  To run, call `./slick demos/hello.sl`

# every expression in a Slick module must be placed under some `def`
# binding--that helps organize code in modules meant to be exported as
# libraries.  By convention, executables (scripts) meant to run via
# `./slick <file>` place all their main code under a `def main:` binding,
# reminiscent of Python's `if __name__ == "__main__":`, but cleaner.
# (The name `main` isn't special--it's just a convention to help you
# organize your code.)

def main:
  print ("hello" ++ " " ++ "world!")
