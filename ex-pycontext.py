
class Beer(object):
    pass

def make_beer():
    return Beer()

class TestMe(unittest.TestCase):
    def test_one(self):
        def inner():
            return make_beer()
        inner()
