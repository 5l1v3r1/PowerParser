#include <string>

using namespace std;

class Assert
{

int mype;

public:

// Constructors
Assert();

// Member Functions
void Equal(bool CorrectValue, bool TestValue, const char *string);
void Equal(int CorrectValue, int TestValue, const char *string);
void Equal(double CorrectValue, double TestValue, const char *string);
void Equal(const char *CorrectValue, string TestValue, const char *string);
};
