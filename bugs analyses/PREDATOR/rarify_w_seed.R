# R function to rarify (subsample) a macroinvertebrate sample down to a fixed count;
# by John Van Sickle, USEPA. email: VanSickle.John@epa.gov    ;
#Version 1.0, 06/10/05;
#Version 1.1, 12/08/14 -- edited by Peter Bryant to set a randon number seed
    #random # seed allows exact same data file to be produced from raw data in each run
        # and limits archiving of input files

#Source this file to current R session;

#See rarify.help.txt for function documentation;
#See rarify.examples.r.txt for examples of usage;

rarify.seed<-function(inbug, sample.ID, abund, subsiz, seed = 42){
   start.time=proc.time();
  outbug<-inbug;
  sampid<-unique(inbug[,sample.ID]);
  nsamp<-length(sampid);
#parameters are set up;
#zero out all abundances in output data set;
outbug[,abund]<-0;
#loop over samples, rarify each one in turn;

for(i in 1:nsamp) { ;
   #extract current sample;
   isamp<-sampid[i];
   flush.console();
   #print(as.character(isamp));
   onesamp<-inbug[inbug[,sample.ID]==isamp,];
   onesamp<-data.frame(onesamp,row.id=seq(1,dim(onesamp)[[1]])); #add sequence numbers as a new column;
   #expand the sample into a vector of individuals;
   samp.expand<-rep(x=onesamp$row.id,times=onesamp[,abund]);
   nbug<-length(samp.expand); #number of bugs in sample;
   #vector of uniform random numbers;
   set.seed(seed)
   ranvec<-runif(n=nbug);
   #sort the expanded sample randomly;
   samp.ex2<-samp.expand[order(ranvec)];
   #keep only the first piece of ranvec, of the desired fised count size;
   #if there are fewer bugs than the fixed count size, keep them all;
   if(nbug>subsiz){subsamp<-samp.ex2[1:subsiz]} else{subsamp<-samp.ex2};
   #tabulate bugs in subsample;
   subcnt<-table(subsamp);
   #define new subsample frame and fill it with new reduced counts;
   newsamp<-onesamp;
   newsamp[,abund]<-0;
   newsamp[match(newsamp$row.id,names(subcnt),nomatch=0)>0,abund]<-as.vector(subcnt);
   outbug[outbug[,sample.ID]==isamp,abund]<-newsamp[,abund];
     }; #end of sample loop;

elaps<-proc.time()-start.time;
print(c("Rarefaction complete. Number of samples = ",nsamp),quote=F);
print(c("Execution time (sec)= ", elaps[1]),quote=F);
outbug; #return subsampled data set as function value;
}; #end of function;
