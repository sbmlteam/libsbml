def endl(*args):
    """
    endl(ostream s) -> ostream

    Insert a newline character into the given C++ stream @p s.

    This is a wrapper around the underlying C++ OStream method
    <code>endl</code>.  It inserts a newline into the stream
    passed as argument.  Additionally, it flushes buffered
    streams.

    @param s the stream to which the newline should be written.

    @return the stream @p s.
    """

def flush(*args):
    """
    flush(ostream s) -> ostream

    Flush the given C++ stream @p s.

    This is a wrapper around the underlying C++ OStream method
    <code>flush</code>.  It flush any pending output in the stream 
    passed as argument.

    @param s the stream to be flushed.

    @return the stream @p s.
    """
