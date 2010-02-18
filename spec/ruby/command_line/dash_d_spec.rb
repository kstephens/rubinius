require File.expand_path('../../spec_helper', __FILE__)

describe "The -d command line option" do
  it "sets $DEBUG to true" do
    ruby_exe("fixtures/debug.rb", :options => "-d", :dir => File.dirname(__FILE__)).chomp.should == "true"
  end
end
