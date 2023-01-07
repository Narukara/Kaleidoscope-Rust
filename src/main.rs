use kaleidoscope::Parser;

fn main() {
    let input = "\
def fun(a, b)
    a + b

extern sin(a)
extern cos(a)

def foo(c)
    fun(c+1, c-1)

foo(9.8)
";
    let mut p = Parser::new(input);
    p.parse();
}
