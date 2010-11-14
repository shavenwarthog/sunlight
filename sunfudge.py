class Fake(fudge.Fake):
    def __init__(self, *args, **kwargs):
        super(Fake,self).__init__(*args, **kwargs)
        self.remember_order()

    def expects(self, call_name, *args, **kwargs):
        if call_name in self._declared_calls:
            self.next_call(for_method=call_name)
        else:
            super(Fake,self).expects(call_name)
        if not args and not kwargs: # emulate fudge.expects()
            return self
        if kwargs.has_key('_returns'):
            self.returns(kwargs.pop('_returns'))
        self.with_args(*args, **kwargs)
        return self

def sun_verify(method):
    @wraps(method)
    @nose.with_setup(fudge.clear_expectations)
    def apply_clear_and_verify(*args, **kw):
        fudge.clear_calls()
        method(*args, **kw)
        fudge.verify() # if no exceptions
    return apply_clear_and_verify

class StringEqual(ValueTest):
    arg_method = 'stringeq'
    def __init__(self, okaystr=None): # pylint: disable=W0231
        self.okaystr = okaystr
    def __eq__(self,other):
        if self.okaystr is None:
            print "StringEqual(%s)" % repr(other)
            return True
        return str(other) == self.okaystr
    def _repr_argspec(self):
        return "(~%s~)" % self.okaystr

