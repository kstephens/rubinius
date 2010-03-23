module Rubinius
  class Sprintf

    attr_accessor :fmt
    attr_accessor :args

    attr_accessor :flags_position
    attr_accessor :flags_space
    attr_accessor :flags_zero
    attr_accessor :flags_star
    attr_accessor :flags_plus
    attr_accessor :flags_minus
    attr_accessor :flags_alternative

    RADIXES = {?b => 2, ?o => 8, ?d => 10, ?x => 16}
    RADIXES.dup.each { | k, v | RADIXES[k.chr.upcase[0]] = v }
    RADIXES.freeze
    ALTERNATIVES = {?o => "0", ?b => "0b", ?B => "0B", ?x => "0x", ?X => "0X"}
    PrecisionMax = 1048576 # Totally random value

    PERCENT = "%".freeze
    SPACE = " ".freeze
    ZERO = "0".freeze
    EMPTY_STRING = "".freeze
    PLUS = "+".freeze
    MINUS = "-".freeze
    STAR = "*".freeze
    HASH = "#".freeze
    DOTDOT = '..'.freeze
    E = 'E'.freeze
    E_LC = 'e'.freeze
    F = 'F'.freeze
    F_LC = 'f'.freeze
    G = 'G'.freeze
    G_LC = 'g'.freeze
    U_LC = "u".freeze
    D_LC = "d".freeze
    I_LC = "i".freeze
    S_LC = "s".freeze
    B    = "B".freeze
    B_LC = "b".freeze
    C_LC = "c".freeze
    O_LC = "o".freeze
    P_LC = "p".freeze
    X    = "X".freeze
    X_LC = "x".freeze
    NAN = "NaN".freeze
    INF = "Inf".freeze
    N_INF = "-Inf".freeze

    def initialize(fmt, *args)
      @tainted = fmt.tainted?
      @fmt, @args, @arg_position = fmt.to_str, args, 0
    end

    def parse
      start = 0
      ret = ""
      width = nil
      precision = nil
      @positional = false
      @relative = false
      @arg_position = 0

      while (match = /%/.match_from(fmt, start))
        @flags_position =
          @flags_space =
          @flags_zero =
          @flags_star =
          @flags_plus =
          @flags_minus =
          @flags_alternative =
          nil

        @width = @precision = @type = nil

        ret << match.pre_match_from(start)
        start = match.begin(0) + 1

        # Special case: %% prints out as "%"
        if (fmt_start = @fmt[start]) == ?\n || fmt_start == 0
          ret << PERCENT << @fmt[start]
          start += 1
          next
        elsif fmt_start == ?% || fmt_start.nil?
          ret << PERCENT
          start += 1
          next
        elsif @fmt[start, 3] =~ /[1-9]\$/ && !@fmt[start + 2]
          ret << PERCENT
          start = @fmt.size
          break
        end

        # FLAG STATE
        while token = /\G( |[1-9]\$|#|\+|\-|0|\*)/.match_from(fmt, start)
          case token[0]
          # Special case: if we get two [1-9]\$, it means that we're outside of flag-land
          when /[1-9]\$/
            raise ArgumentError, "value given twice - #{token[0]}" if @flags_position
            @flags_position = token[0][0].chr.to_i
            start += 1
          when SPACE
            @flags_space = true
          when HASH
            @flags_alternative = true
          when PLUS
            @flags_plus = true
          when MINUS
            @flags_minus = true
          when ZERO
            @flags_zero = true
          when STAR
            raise ArgumentError, "width given twice" if @flags_star
            if width_dollar_match = /\G[1-9]\$/.match_from(fmt, start + 1)
              @width = Slot.new("*" << width_dollar_match[0])
              start += 2
            end
            @flags_star = true
          end
          start += 1
        end

        # WIDTH STATE
        if !@flags_star && width_match = /\G([1-9]\$|\*|\d+)/.match_from(fmt, start)
          @width = Slot.new(width_match[0])
          start += width_match[0].size
        end

        # PRECISION DETERMINATION STATE
        if /\G\./.match_from(fmt, start)
          start += 1
          # PRECISION STATE
          if /\G\*/.match_from(fmt, start)
            if precision_dollar_match = /\G[1-9]\$/.match_from(fmt, start + 1)
              @precision = Slot.new("*" << precision_dollar_match[0])
              start += 3
            else
              @precision = Slot.new(STAR)
              start += 1
            end
          elsif precision_match = /\G([1-9]\$|\d+)/.match_from(fmt, start)
            @precision = Slot.new(precision_match[0])
            start += precision_match[0].size
          else
            @precision = Slot.new(ZERO)
          end

          # check for positional value again, after the optional '.'
          if positional_match = /\G[1-9]\$/.match_from(fmt, start)
            raise ArgumentError, "value given twice - #{token[0]}" if @flags_position
            @flags_position = positional_match[0][0].chr.to_i
            start += 2
          end
        end

        # TYPE STATE
        unless type = /\G[bcdEefGgiopsuXx]/i.match_from(fmt, start)
          raise ArgumentError, "malformed format string - missing field type"
        else
          @type = type[0]
          start += 1
        end

        # Next: Use the parsed values to format some stuff :)
        f = format
        ret << f if f
      end
      if $DEBUG == true && !@positional
        raise ArgumentError, "you need to use all the arguments" unless @arg_position == @args.size
      end
      ret << @fmt[start..-1] if start < @fmt.size
      ret.taint if @tainted
      ret
    end

    def format
      # GET VALUE
      if @flags_position
        val = Slot.new("#{@flags_position}$")
        val = get_arg(val)
      end

      # GET WIDTH
      @width = Slot.new(STAR) if @flags_star && !@width
      width = get_arg(@width)
      width = width.to_int if width.respond_to?(:to_int)
      if width && width < 0
        width = width.abs
        @flags_minus = true
      end

      # GET PRECISION
      precision = get_arg(@precision)
      precision = precision.to_int if precision.respond_to?(:to_int)

      unless @flags_position
        val = Slot.new(STAR)
        val = get_arg(val)
      end

      case typec = @type[0]
      when ?e, ?E, ?f, ?g, ?G
        if (typec == ?g || typec == ?G) && @flags_alternative
          @old_type = G_LC
          @type = F_LC
          precision = 4 unless precision
        end
        val = Float(val)
        if val.finite?
          ret = val.send(:to_s_formatted, build_format_string(width, precision))
          ret = plus_char + ret if val >= 0 && @old_type
        else
          ret = (val < 0 ? N_INF : INF) if val.infinite?
          ret = NAN if val.nan?
          ret = plus_char + ret if val > 0
          @flags_zero = @flags_space = @flags_plus = nil
          ret = pad(ret, width, precision)
        end
      when ?u
        val = get_number(val)
        if val < 0
          unless val.kind_of?(Fixnum)
            raise ArgumentError, "invalid type (only Fixnum allowed)"
          end

          plus_or_space = @flags_space || @flags_plus
          unless plus_or_space
            val = (1 << (2.size * 8)) + val
          end
          unless @flags_zero or precision or plus_or_space
            ret = "..#{pad(val, width, precision)}"
          else
            ret = pad(val, width, precision)
          end
        else
          ret = pad(val, width, precision)
        end
      when ?d, ?i
        val = get_number(val)
        ret = pad(val, width, precision)
      when ?c
        val = val.to_int if val.respond_to?(:to_int)
        raise TypeError, "cannot convert #{val.class} into Integer" unless val.respond_to?(:chr) && val.respond_to?(:%)
        val = (val % 256).chr
        ret = pad(val, width, precision)
      when ?s
        @flags_zero = @flags_space = @flags_plus = nil
        ret = pad(val, width, precision)
        ret.taint if val.tainted?
      when ?p
        @flags_zero = @flags_space = @flags_plus = nil
        ret = pad(val.inspect, width, precision)
      when ?o, ?x, ?X, ?b, ?B
        val = get_number(val)
        unless @flags_space || @flags_plus
          ret = Number.new(val, RADIXES[typec]).rep
          chr = val < 0 ? (RADIXES[typec] - 1).to_s(RADIXES[typec]) : ZERO
          ret = pad(ret, width, precision, chr)
          ret = (ALTERNATIVES[typec] ||= ''.freeze) + ret if @flags_alternative
        else
          @flags_plus = nil if val < 0
          ret = val.to_s(RADIXES[typec])
          ret.gsub!(/^(\-?)/, "\1#{ALTERNATIVES[typec]}") if @flags_alternative
          ret = pad(ret, width, precision)
          ret.gsub!(/ \-/, MINUS)
        end
        ret = ret.downcase if typec == ?x
        ret = ret.upcase if typec == ?X
      end
      ret
    end

    def get_number(val)
      unless val.respond_to?(:full_to_i)
        if val.respond_to?(:to_int)
          val = val.to_int
        elsif val.respond_to?(:to_i)
          val = val.to_i
        end
      end
      val = val.full_to_i if val.respond_to?(:full_to_i)
      val = 0 if val.nil?
      val
    end

    def build_format_string(width, precision)
      ret = "%#{make_flags}#{width}"
      ret << ".#{precision}" if precision
      ret << @type
      ret
    end

    def make_flags
      ret = ""
      ret << SPACE if @flags_space
      ret << HASH  if @flags_alternative
      ret << PLUS  if @flags_plus
      ret << MINUS if @flags_minus
      ret << ZERO  if @flags_zero
      ret
    end

    def get_arg(slot)
      return nil unless slot

      case
      when slot.position == :next
        raise ArgumentError, "unnumbered mixed with numbered" if @positional
        @relative = true
        raise ArgumentError, "you ran out of arguments" if @arg_position >= @args.size
        ret = @args[@arg_position]
        @arg_position += 1
      when slot.pos
        raise ArgumentError, "unnumbered mixed with numbered" if @relative
        @positional = true
        ret = @args[slot.position - 1]
      when slot.value
        raise ArgumentError, "unnumbered mixed with numbered" if @positional
        @relative = true
        ret = slot.value
      else
        raise ArgumentError, "argument position does not exist: #{slot.str}"
      end

      ret
    end

    def pad(val, width, precision, pad_override = nil)
      direction = @flags_minus ? :ljust : :rjust
      ret = val.to_s
      modded_width = width.to_i + (@flags_plus ? 1 : 0)
      width = nil if modded_width <= val.to_s.size
      if ret[0] != ?-
        sign = plus_char
      else
        sign = EMPTY_STRING
      end

      if precision || @flags_zero
        ret.gsub!(DOTDOT, EMPTY_STRING)
      end
      if precision
        if precision > PrecisionMax
          raise ArgumentError, "precision too big"
        end
        ret = sign + ret.send(direction, precision, pad_override || ZERO)
        @flags_zero = @flags_plus = @flags_space = nil
      end
      if width
        if pad_char != SPACE && ret[0] == ?-
          ret.slice!(0)
          sign = MINUS
          width -= 1
          ret = ret.rjust(width, pad_char)
        else
          ret = ret.send(direction, width, pad_char)
          ret[0] = sign unless sign.empty?
          return ret
        end
      end
      sign + ret
    end

    def pad_char
      @flags_zero ? ZERO : SPACE
    end

    def plus_char
      return PLUS if @flags_plus
      return SPACE if @flags_space
      EMPTY_STRING
    end

    class Slot

      # pos means it got a N$ position
      attr_reader :pos
      attr_reader :position
      attr_reader :value
      attr_reader :str

      INSTANCE_CACHE = { }
      def self.new str
        INSTANCE_CACHE[str] ||=
          super(str).freeze
      end

      def initialize(str)
        @pos = false
        @str = str
        if str.size == 3 && /\*\d\$/.match(str)
          @pos = true
          @position = str[1..1].to_i
        elsif str.size == 2 && str[1] == ?$
          @pos = true
          @position = str[0..0].to_i
        elsif str == STAR
          @position = :next
        else
          @value = str.to_i
        end
      end
    end

    class Number
      attr_reader :number, :radix, :pad

      def initialize(number, radix)
        @number = number
        @radix = radix
        @pad = (radix - 1).to_s(radix)
      end

      def rep
        return @number.to_s(@radix) if(@number >= 0) || @radix == 10
        strlen = (@number.to_s(@radix)).size
        max = (@pad * strlen).to_i(@radix)
        DOTDOT + (max + @number + 1).to_s(@radix)
      end

    end

  end
end
