#include <v8.h>

extern "C" {
class GlobalRef {
public:
  GlobalRef(){
  }
  ~ GlobalRef() { }

  v8::Global<v8::Context>& getContext() {return context_;}
private:
  v8::Global<v8::Context> context_;
};

}
