#include <cstring>
#ifdef HAVE_GENMALLOC
#include "genmalloc/genmalloc.h"
#endif
#include "PowerParser.hh"
#include "AssertStuff.hh"

using namespace PP;

using namespace std;

int main(int argc, char **argv)
{
   // Create the parser
   PowerParser *parse = new PowerParser();
   int mype = parse->comm->getProcRank();
   int npes = parse->comm->getNumProcs();

   Assert *assert = new Assert();

   if (mype == 0) printf("\n\t\tRunning the Parser read tests\n\n");

   // Process input file
   parse->parse_file("parsetest.in");

   int iret;
   parse->compile_buffer(iret);
   parse->echo_input_start();

   vector<string> sinput(1);
   parse->get_char("string_input", sinput);
   assert->Equal("parsetest", sinput[0], "Read string");

   int intvalue = -1;
   parse->get_int("int_input", &intvalue);
   assert->Equal(10, intvalue, "Read int");
   
   double doublevalue = -1;
   parse->get_real("double_input", &doublevalue);
   assert->Equal(5.5, doublevalue, "Read real");

   doublevalue = -1;
   parse->get_real("dx", &doublevalue);
   assert->Equal(1.0, doublevalue, "Read real");

   doublevalue = -1;
   parse->get_real("xreal", &doublevalue);
   assert->Equal(1.0, doublevalue, "Read real");

   intvalue = -1;
   parse->get_int("ivalue", &intvalue);
   assert->Equal(100, intvalue, "Read int");
   
   parse->get_char("string", sinput);
   assert->Equal("malarky", sinput[0], "Read string");

   bool boolvalue = false;
   parse->get_bool("doThing", &boolvalue);
   assert->Equal(true, boolvalue, "Read boolean");

   boolvalue = false;
   parse->get_bool("doThing1", &boolvalue);
   assert->Equal(true, boolvalue, "Read boolean");

   boolvalue = false;
   parse->get_bool("doThing2", &boolvalue);
   assert->Equal(true, boolvalue, "Read boolean");

   boolvalue = true;
   parse->get_bool("doThing3", &boolvalue);
   assert->Equal(false, boolvalue, "Read boolean");

   boolvalue = true;
   parse->get_bool("doThing4", &boolvalue);
   assert->Equal(false, boolvalue, "Read boolean");

   //**************************************
   // Start of array tests
   //**************************************
 
   bool skip = false;
   vector<int> size;

   double doublearray[6] = {-1.0, -1.0, -1.0, -1.0, -1.0, -1.0};
   size.push_back(6);
   parse->get_real("array1d", doublearray, size, skip);

   double dcheckarray[6] = {1.0, 2.3, -5.6, 7.1e19, 3.0, -3.4e-23};

   bool iflag = false;
   for (int i = 0; i < 6; i++){
      if (doublearray[i] != dcheckarray[i]) iflag = true;
   }

   if (! iflag){
      if (mype == 0) {
         printf("PASSED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
         printf("     should be real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                dcheckarray[0], dcheckarray[1], dcheckarray[2], dcheckarray[3], dcheckarray[4], dcheckarray[5]);
      }
   }

   for (int i = 0; i < 6; i++){
      doublearray[i] = -1.0;
   }
   parse->get_real("array1d_1", doublearray, size, skip);

   for (int i = 0; i < 6; i++){
      if (doublearray[i] != dcheckarray[i]) iflag = true;
   }

   if (! iflag){
      if (mype == 0) {
         printf("PASSED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
         printf("     should be real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                dcheckarray[0], dcheckarray[1], dcheckarray[2], dcheckarray[3], dcheckarray[4], dcheckarray[5]);
      }
   }

   for (int i = 0; i < 6; i++){
      doublearray[i] = -1.0;
   }
   parse->get_real("array1d_2", doublearray, size, skip);

   for (int i = 0; i < 6; i++){
      if (doublearray[i] != dcheckarray[i]) iflag = true;
   }

   if (! iflag){
      if (mype == 0) {
         printf("PASSED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
         printf("     should be real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                dcheckarray[0], dcheckarray[1], dcheckarray[2], dcheckarray[3], dcheckarray[4], dcheckarray[5]);
      }
   }

   for (int i = 0; i < 6; i++){
      doublearray[i] = -1.0;
   }
   parse->get_real("array1d_3", doublearray, size, skip);

   for (int i = 0; i < 6; i++){
      if (doublearray[i] != dcheckarray[i]) iflag = true;
   }

   if (! iflag){
      if (mype == 0) {
         printf("PASSED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
      }
   } else {
      if (mype == 0) {
         printf("  FAILED: Read real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                doublearray[0], doublearray[1], doublearray[2], doublearray[3], doublearray[4], doublearray[5]);
         printf("     should be real[0] %lg real[1] %lg real[2] %lg real[3] %lg real[4] %lg real[5] %lg\n",
                dcheckarray[0], dcheckarray[1], dcheckarray[2], dcheckarray[3], dcheckarray[4], dcheckarray[5]);
      }
   }

#ifdef HAVE_GENMALLOC
   double **doublearray2d = (double **)genmatrix(2, 3, sizeof(double));

   size.clear();
   size.push_back(3);
   size.push_back(2);
   parse->get_real("array2d", &doublearray2d[0][0], size, skip);

   double **dcheckarray2d = (double **)genmatrix(2, 3, sizeof(double));

   dcheckarray2d[0][0] = 1.0;
   dcheckarray2d[0][1] = 2.3;
   dcheckarray2d[0][2] = -5.6;
   dcheckarray2d[1][0] = 7.1e19;
   dcheckarray2d[1][1] = 3.0;
   dcheckarray2d[1][2] = -3.4e-23;
   
   for (int j = 0; j < size[1]; j++){
      for (int i = 0; i < size[0]; i++){
         if (doublearray2d[j][i] != dcheckarray2d[j][i]) iflag = true;
      }
   }

   if (! iflag){
      printf("PASSED: Read real[0][0] %lg real[0][1] %lg real[0][2] %lg real[1][0] %lg real[1][1] %lg real[1][2] %lg\n",
             doublearray2d[0][0], doublearray2d[0][1], doublearray2d[0][2],
             doublearray2d[1][0], doublearray2d[1][1], doublearray2d[1][2]);
   } else {
      printf("  FAILED: Read real[0][0] %lg real[0][1] %lg real[0][2] %lg real[1][0] %lg real[1][1] %lg real[1][2] %lg\n",
             doublearray2d[0][0], doublearray2d[0][1], doublearray2d[0][2],
             doublearray2d[1][0], doublearray2d[1][1], doublearray2d[1][2]);
      printf("     should be real[0][0] %lg real[0][1] %lg real[0][2] %lg real[1][0] %lg real[1][1] %lg real[1][2] %lg\n",
             dcheckarray2d[0][0], dcheckarray2d[0][1], dcheckarray2d[0][2],
             dcheckarray2d[1][0], dcheckarray2d[1][1], dcheckarray2d[1][2]);
   }

   genmatrixfree((void **)doublearray2d);
   genmatrixfree((void **)dcheckarray2d);

   doublearray2d = (double **)genmatrix(3, 2, sizeof(double));

   size.clear();
   size.push_back(2);
   size.push_back(3);
   parse->get_real("array2d_1", &doublearray2d[0][0], size, skip);

   dcheckarray2d = (double **)genmatrix(3, 2, sizeof(double));

   dcheckarray2d[0][0] = 1.0;
   dcheckarray2d[0][1] = 2.3;
   dcheckarray2d[1][0] = -5.6;
   dcheckarray2d[1][1] = 7.1e19;
   dcheckarray2d[2][0] = 3.0;
   dcheckarray2d[2][1] = -3.4e-23;
   
   for (int j = 0; j < size[1]; j++){
      for (int i = 0; i < size[0]; i++){
         if (doublearray2d[j][i] != dcheckarray2d[j][i]) iflag = true;
      }
   }

   if (! iflag){
      printf("PASSED: Read real[0][0] %lg real[0][1] %lg real[1][0] %lg real[1][1] %lg real[2][0] %lg real[2][1] %lg\n",
             doublearray2d[0][0], doublearray2d[0][1],
             doublearray2d[1][0], doublearray2d[1][1],
             doublearray2d[2][0], doublearray2d[2][1]);
   } else {
      printf("  FAILED: Read real[0][0] %lg real[0][1] %lg real[1][0] %lg real[1][1] %lg real[2][0] %lg real[2][1] %lg\n",
             doublearray2d[0][0], doublearray2d[0][1],
             doublearray2d[1][0], doublearray2d[1][1],
             doublearray2d[2][0], doublearray2d[2][1]);
      printf("     should be real[0][0] %lg real[0][1] %lg real[1][0] %lg real[1][1] %lg real[2][0] %lg real[2][1] %lg\n",
             dcheckarray2d[0][0], dcheckarray2d[0][1],
             dcheckarray2d[1][0], dcheckarray2d[1][1],
             dcheckarray2d[2][0], dcheckarray2d[2][1]);
   }

   genmatrixfree((void **)doublearray2d);
   genmatrixfree((void **)dcheckarray2d);
#endif

   delete parse;

   if (mype == 0) printf("\n\t\tFinished the Parser read tests\n\n");
   
}

