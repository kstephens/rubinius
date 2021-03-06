module Kernel

  ##
  #--
  # HACK todo handle cascading raises (ie, TypeError raise
  # raising forever blows)
  #++

  def raise(exc=undefined, msg=undefined, ctx=nil)
    skip = false
    if exc.equal? undefined
      exc = $!
      if exc
        skip = true
      else
        exc = RuntimeError.new("No current exception")
      end
    elsif exc.respond_to? :exception
      if msg.equal? undefined
        exc = exc.exception
      else
        exc = exc.exception msg
      end
      raise ::TypeError, 'exception class/object expected' unless exc.kind_of?(::Exception)
    elsif exc.kind_of? String or !exc
      exc = ::RuntimeError.exception exc
    else
      raise ::TypeError, 'exception class/object expected'
    end

    unless skip
      exc.set_context ctx if ctx
      exc.capture_backtrace!(2) unless exc.backtrace?
    end

    if $DEBUG and $VERBOSE != nil
      if loc = exc.locations
        pos = loc[1].position
      else
        pos = Rubinius::VM.backtrace(1)[0].position
      end

      STDERR.puts "Exception: `#{exc.class}' #{pos} - #{exc.message}"
    end

    Rubinius.raise_exception exc
  end
  module_function :raise

  alias_method :fail, :raise
  module_function :fail

  def method_missing(meth, *args)
    cls = NoMethodError

    case Rubinius.method_missing_reason
    when :private
      msg = "private method '#{meth}'"
    when :protected
      msg = "protected method '#{meth}'"
    when :super
      msg = "no superclass method '#{meth}'"
    when :vcall
      msg = "no method or variable '#{meth}'"
      cls = NameError
    else
      msg = "no method '#{meth}'"
    end

    if __kind_of__(Module)
      msg << " on #{self} (#{self.class})"

    # A seperate case for nil, because people like to patch methods to
    # nil, so we can't call methods on it reliably.
    elsif nil.equal?(self)
      msg << " on nil:NilClass."
    elsif ImmediateValue === self
      msg << " on #{self}:#{self.class}."
    else
      msg << " on an instance of #{self.class}."
    end

    Kernel.raise cls.new(msg, meth, args)
  end

  private :method_missing

  # Add in $! in as a hook, to just do $!. This is for accesses to $!
  # that the compiler can't see.
  Rubinius::Globals.set_hook(:$!) { $! }

  # Same as $!, for any accesses we might miss.
  # HACK. I doubt this is correct, because of how it will be called.
  Rubinius::Globals.set_hook(:$~) { Regexp.last_match }

  Rubinius::Globals.set_hook(:$*) { ARGV }

  Rubinius::Globals.set_hook(:$@) { $! ? $!.backtrace : nil }

  Rubinius::Globals.set_hook(:$$) { Process.pid }

  write_filter = proc do |key, io|
    unless io.respond_to? :write
      raise ::TypeError, "#{key} must have write method, #{io.class} given"
    end
    io
  end

  Rubinius::Globals.add_alias :$stdout, :$>
  Rubinius::Globals.set_filter(:$stdout, write_filter)
  Rubinius::Globals.set_filter(:$stderr, write_filter)

  get = proc do
    warn "$defout is obsolete; it will be removed any day now"
    $stdout
  end

  set = proc do |key, io|
    warn "$defout is obsolete; it will be removed any day now"
    $stdout = io
  end

  Rubinius::Globals.set_hook(:$defout, get, set)

  get = proc do
    warn "$deferr is obsolete; it will be removed any day now"
    $stderr
  end

  set = proc do |key, io|
    warn "$deferr is obsolete; it will be removed any day now"
    $stderr = io
  end

  Rubinius::Globals.set_hook(:$deferr, get, set)

  # Proper kcode support
  get = proc { Rubinius.kcode.to_s }
  set = proc { |key, val| Rubinius.kcode = val }
  Rubinius::Globals.set_hook(:$KCODE, get, set)

  # Implements rb_path2name. Based on code from wycats
  def const_lookup(name)
    names = name.split '::'
    names.shift if names.first.empty?
    names.inject(Object) do |m, n|
      m.const_defined?(n) ? m.const_get(n) : m.const_missing(n)
    end
  end

  private :const_lookup
end
