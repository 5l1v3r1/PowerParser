#include <stdio.h>
#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include "AssertStuff.hh"

Assert::Assert()
{
   mype = 0;
#ifdef HAVE_MPI
   MPI_Comm_rank(MPI_COMM_WORLD, &mype);
#endif
}

void Assert::Equal(bool CorrectValue, bool TestValue, const char *string)
{
   if (CorrectValue == TestValue){
      if (mype == 0) {
         printf("PASSED: %s %s\n", string, TestValue?"true":"false");
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: %s %s should be %s\n", string,
            TestValue?"true":"false", CorrectValue?"true":"false");
      }
   }
}

void Assert::Equal(int CorrectValue, int TestValue, const char *string)
{
   if (CorrectValue == TestValue){
      if (mype == 0) {
         printf("PASSED: %s %d\n", string, TestValue);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: %s %d should be %d\n", string, TestValue, CorrectValue);
      }
   }
}

void Assert::Equal(double CorrectValue, double TestValue, const char *string)
{
   if (CorrectValue == TestValue){
      if (mype == 0) {
         printf("PASSED: %s %lf\n", string, TestValue);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: %s %lf should be %lf\n", string, TestValue, CorrectValue);
      }
   }
}

void Assert::Equal(const char *CorrectValue, string TestValue, const char *string)
{
   if (TestValue.compare(CorrectValue) == 0){
      if (mype == 0) {
         printf("PASSED: %s %s\n", string, TestValue.c_str());
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: %s %s should be %s\n", string, TestValue.c_str(), CorrectValue);
      }
   }
}
