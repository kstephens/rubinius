require File.expand_path('../../spec_helper', __FILE__)
require File.expand_path('../fixtures/super', __FILE__)

describe "The super keyword" do
  it "calls the method on the calling class" do
    Super::S1::A.new.foo([]).should == ["A#foo","A#bar"]
    Super::S1::A.new.bar([]).should == ["A#bar"]
    Super::S1::B.new.foo([]).should == ["B#foo","A#foo","B#bar","A#bar"]
    Super::S1::B.new.bar([]).should == ["B#bar","A#bar"]
  end
  
  it "searches the full inheritence chain" do
    Super::S2::B.new.foo([]).should == ["B#foo","A#baz"]
    Super::S2::B.new.baz([]).should == ["A#baz"]
    Super::S2::C.new.foo([]).should == ["B#foo","C#baz","A#baz"]
    Super::S2::C.new.baz([]).should == ["C#baz","A#baz"]
  end

  it "searches class methods" do
    Super::S3::A.new.foo([]).should == ["A#foo"]
    Super::S3::A.foo([]).should == ["A::foo"]
    Super::S3::A.bar([]).should == ["A::bar","A::foo"]
    Super::S3::B.new.foo([]).should == ["A#foo"]
    Super::S3::B.foo([]).should == ["B::foo","A::foo"]
    Super::S3::B.bar([]).should == ["B::bar","A::bar","B::foo","A::foo"]
  end

  it "calls the method on the calling class including modules" do
    Super::MS1::A.new.foo([]).should == ["ModA#foo","ModA#bar"]
    Super::MS1::A.new.bar([]).should == ["ModA#bar"]
    Super::MS1::B.new.foo([]).should == ["B#foo","ModA#foo","ModB#bar","ModA#bar"]
    Super::MS1::B.new.bar([]).should == ["ModB#bar","ModA#bar"]
  end
  
  it "searches the full inheritence chain including modules" do
    Super::MS2::B.new.foo([]).should == ["ModB#foo","A#baz"]
    Super::MS2::B.new.baz([]).should == ["A#baz"]
    Super::MS2::C.new.baz([]).should == ["C#baz","A#baz"]
    Super::MS2::C.new.foo([]).should == ["ModB#foo","C#baz","A#baz"]
  end

  it "searches class methods including modules" do
    Super::MS3::A.new.foo([]).should == ["A#foo"]
    Super::MS3::A.foo([]).should == ["ModA#foo"]
    Super::MS3::A.bar([]).should == ["ModA#bar","ModA#foo"]
    Super::MS3::B.new.foo([]).should == ["A#foo"]
    Super::MS3::B.foo([]).should == ["B::foo","ModA#foo"]
    Super::MS3::B.bar([]).should == ["B::bar","ModA#bar","B::foo","ModA#foo"]
  end

  it "calls the correct method when the method visibility is modified" do
    Super::MS4::A.new.example.should == 5
  end
  
  it "calls the correct method when the superclass argument list is different from the subclass" do
    Super::S4::A.new.foo([]).should == ["A#foo"]
    Super::S4::B.new.foo([],"test").should == ["B#foo(a,test)", "A#foo"]
  end

  ruby_bug "#1151 [ruby-core:22040]", "1.8.7.174" do
    it "raises an error error when super method does not exist" do
      sup = Class.new
      sub_normal = Class.new(sup) do
        def foo
          super()
        end
      end
      sub_zsuper = Class.new(sup) do
        def foo
          super
        end
      end

      lambda {sub_normal.new.foo}.should raise_error(NoMethodError, /super/)
      lambda {sub_zsuper.new.foo}.should raise_error(NoMethodError, /super/)
    end
  end

  it "calls the superclass method when in a block" do
    Super::S6.new.here.should == :good
  end

  it "calls the superclass method when initial method is defined_method'd" do
    Super::S7.new.here.should == :good
  end

  it "supers up appropriate name even if used for multiple method names" do
    sup = Class.new do
      def a; "a"; end
      def b; "b"; end
    end

    sub = Class.new(sup) do
      [:a, :b].each do |name|
        define_method name do
          super()
        end
      end
    end

    sub.new.a.should == "a"
    sub.new.b.should == "b"
    sub.new.a.should == "a"
  end

  ruby_version_is ""..."1.9" do
    it "can be used with implicit arguments from a method defined with define_method" do
      sup = Class.new do
        def a; "a"; end
      end

      sub = Class.new(sup) do
        define_method :a do
          super
        end
      end

      sub.new.a.should == "a"
    end
  end

  ruby_version_is "1.9" do
    it "can't be used with implicit arguments from a method defined with define_method" do
      Class.new do
        define_method :a do
          super
        end.should raise_error(RuntimeError)
      end
    end
  end

  # Rubinius ticket github#157
  it "calls method_missing when a superclass method is not found" do
    lambda {
      Super::MM_B.new.is_a?(Hash).should == false
    }.should_not raise_error(NoMethodError)
  end

  # Rubinius ticket github#180
  it "respects the original module a method is aliased from" do
    lambda {
      Super::Alias3.new.name3.should == [:alias2, :alias1]
    }.should_not raise_error(RuntimeError)
  end
end
