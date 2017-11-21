package prog1;
	
	import Vector::* ;
	import FIFOF ::* ;
	import FixedPoint::*;
	import DefaultValue ::*;
	
	
	interface Tcm_cluster_ifc;
		method Action read_MPKI( Vector#(8,Bit#(10)) mpki );			// read mpki on start of every quantum
		method Action read_BW_usage(Vector#(8,Bit#(17)) bwUsage);	// read bandwidth usage on start of every quantum
		method Action read_request(Request_from_memory buffer);		// read memory request one by one
	endinterface : Tcm_cluster_ifc
	
	/*
		PHYSICAL ADDRESS
			Channel (1 bit)				: A33
			Ranks (2 bits)				: A32 - A31
			row address (15 bits) : A30 - A16 
			Bank (3 bits)					: A15 - A13
			Column (10 bits)			: A12 - A3
														: A2 - A0
			
	*/
	typedef struct																							// structure of request form memoby
	{
		Bit#(3) thread_id;																				// hardware thread id of cores
		Bit#(34) physical_address;																// ram address 
		Bit#(2) type_of_request;					
		Bit#(64) data;																						// data to write
	}
	Request_from_memory deriving(Bits,Eq);
	
	instance DefaultValue #(Request_from_memory);
		defaultValue = Request_from_memory{
																			thread_id: 0,
																			physical_address: 0,
																			type_of_request : 0,
																			data : 0
																			};
	endinstance
	
	
(* synthesize *)
	module dram_scheduler (Tcm_cluster_ifc);
	
	/*
		8 cores
		8 banks
		
	*/				
		Vector#(9,Reg#( Bit#(4))) latency_cluster <- replicateM(mkReg(-1));	// to store -1; contains thread ids
		Vector#(8,Reg#( Bit#(1))) bandwidth_cluster <- replicateM(mkReg(1));	// contains 1 for presence of thread in cluster else 0
		
		Vector#(8,FIFOF#(Request_from_memory) ) req_fifo ;	// memory req for each thread
		
		FIFOF#(Request_from_memory) buffer_fifo <- mkFIFOF;	// all memory req
		
		for(int i = 0; i<8 ; i = i+1)
			req_fifo[i] <- mkFIFOF;			//for time stamp FCFS 
		
		Reg#(Bit#(17)) cnt <- mkReg(100000);		// for cycles , 10^6 = 1 quantum
		Reg#(Bit#(17)) sumBW <- mkReg(0);				// used in clustering for cummulative BW usage
		Reg#(Bit#(17)) totalBW <- mkReg(0);			// total BW usage of quantum
		Reg#(Bit#(4)) id <- mkReg(0);						// used in find_min for find id with min mpki
		Reg#(Bit#(3)) thread_count <- mkReg(0);	// counting no. of threads in clustering 
		Reg#(Bit#(3)) idx <- mkReg(0);
		Reg#(Bit#(10)) min <- mkReg(0);
		Reg#(Bit#(1)) min_flag <- mkReg(0);
		Reg#(Bit#(1)) thead_id_flag <- mkReg(0);
		Reg#(Bit#(1)) mpki_flag <- mkReg(0);
		Reg#(Bit#(1)) bwUsage_flag <- mkReg(0);
		Reg#(Bit#(1)) blp_counter_flag <- mkReg(0);
		Reg#(Bit#(1)) bandwidth_cluster_flag <- mkReg(0);
		Reg#(Bit#(1)) thread_request_flag <- mkReg(1);
		Reg#(Bit#(1)) calc_blp_matrix_flag <- mkReg(1);
		Reg#(Bit#(1)) calc_rbl_matrix_flag <- mkReg(1);
		Reg#(Bit#(1)) quanta_flag <- mkReg(0);
		Reg#(Request_from_memory) buffer_1 <- mkReg(defaultValue);
		
		
		/*
			for 1-d vector
				indices correponds to thread_id except for latency_cluster vector
				
			for 2-d matrix
			row : thread_id  , column : banks
		*/
		
		
		
		Vector#(8,Vector#(128,Reg#(Request_from_memory))) request_buffer_matrix <- replicateM( replicateM(mkReg(defaultValue)) );
									
		/* init left*/																																												
		Vector#(8,Reg#(Bit#(7))) request_buffer_matrix_idx <- replicateM(mkReg(0));
		Vector#(8,Vector#(8,Reg#(Bit#(7)))) last_access_req_id <- replicateM(replicateM(mkReg(0)));		
		Vector#(8,Reg#(Bit#(7))) row_hit_order <- replicateM(mkReg(0));
		Vector#(8,Reg#(Bit#(7))) row_hit_order_idx <- replicateM(mkReg(0));
		
				
		Vector#(8,Reg#(Bit#(10))) mpki_1 <- replicateM( mkReg(0) );		//for mpki values via interface
		Vector#(8,Reg#(Bit#(17))) bwUsage_1 <- replicateM( mkReg(0) );//for bw usage values via interface
		Vector#(8,Reg#(Bit#(1)) )visited_mpki_1 <- replicateM( mkReg(0) );
		Vector#(8,Vector#(8,Reg#(Bit#(6)))) blp_matrix <- replicateM(replicateM(mkReg(0)));
		Vector#(8,Vector#(8,Reg#(Bit#(15)))) rbl_address_matrix <- replicateM(replicateM(mkReg(0)));
		Vector#(8,Vector#(8,Reg#(Bit#(15)))) rbl_matrix <- replicateM(replicateM(mkReg(0)));
		Vector#(8,Reg#(Bit#(3))) blp_counter <- replicateM(mkReg(0));
		Vector#(8,Reg#(FixedPoint#(2,10))) rbl_avg <- replicateM( mkReg(0));
		Vector#(8,Reg#(FixedPoint#(2,10))) total_thread_request <- replicateM(mkReg(0));		// compile error
		
		
/*---------------------RULES--------------------------------------*/
		
		/*
			we start with rule find_min and the keep switching btw rule find_min and rule clustering for clustering the threads into latency sensitive and bandwidth sensidtive
		*/
		rule find_min(min_flag == 1 && mpki_flag == 1 && bwUsage_flag == 1);	
			Bit#(4) id_temp  = 0;
			Bit#(10) min_temp = 1023 ;
			for(Bit#(4) i = 0; i<=7; i = i+1)
			begin
				if(mpki_1[i] < min_temp && visited_mpki_1[i] == 0)
				begin
					min_temp = mpki_1[i];
					id_temp = i;//[2:0]; 
				end
			end
			
		id <= id_temp;
		min <= min_temp;
		visited_mpki_1[id] <= 1;
		min_flag <= 0;
		endrule : find_min
		
		rule clustering( thread_count <= 7 && min_flag == 0 && bandwidth_cluster_flag == 0 );
			sumBW <= sumBW + bwUsage_1[id];
			if( sumBW <= (totalBW>>1 ))		// thresh_cluster = 4/8 (0.5)
			begin
				latency_cluster[idx] <= id;	
				idx <= idx + 1;
				min_flag <= 1;
			end
			else
			begin
				bandwidth_cluster_flag <= 1;	
				latency_cluster[idx] <= -1;
			end
			
			thread_count <= thread_count + 1; // check and store -1
		endrule : clustering
	
	
	/*
		helps in finding the bandwith cluter by traversing the latency sesitive cluster and marking the bandwidth sensitive cluster
	*/
		rule bandwidth_clustering ( bandwidth_cluster_flag == 1);
			Vector#(8,Bit#(1)) bandwidth_cluster_temp = replicate(1);// <- replicate(mkReg(0));
			for(int i=0; i<=7; i= i+1)
			begin
				if(latency_cluster[i] != -1)
					bandwidth_cluster_temp[latency_cluster[i]] = 0;
			end
			
			for(int i=0; i<=7; i= i+1)
				bandwidth_cluster[i] <= bandwidth_cluster_temp[i];
				
		bandwidth_cluster_flag <= 0;
		endrule : bandwidth_clustering
		
		
	/*
		fills each fifo with the request 
	*/
		rule thread_request(thread_request_flag == 0);	//condition
			//req_fifo[buffer_1.thread_id].enq(buffer_1);
			total_thread_request[buffer_1.thread_id] <= total_thread_request[buffer_1.thread_id] + 1;
			
			request_buffer_matrix[buffer_1.thread_id][request_buffer_matrix_idx[buffer_1.thread_id]] <= buffer_1;
			request_buffer_matrix_idx[buffer_1.thread_id] <= request_buffer_matrix_idx[buffer_1.thread_id] + 1;
			// from 0 to request_buffer_matrix_idx[buffer_1.thread_id] use it for traversing
			
			
			thread_request_flag <= 1;
		endrule : thread_request
		
		
	/*
		calculates blp_matrix and blp_counter
		each entry int bpl_mattix contains no. of bank's req
		each entry in blp_counter contains the no. of bank requests of each thread 
	*/
		rule calc_blp_matrix(calc_blp_matrix_flag == 0);	//condition
			blp_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] 	
			<= blp_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] + 1;
			
			if(blp_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] == 0)
				blp_counter[buffer_1.thread_id] <= blp_counter[buffer_1.thread_id] + 1;
				
			calc_blp_matrix_flag <= 1;
		endrule : calc_blp_matrix
		
		
	/*
		fills rbl_matrix nad calculates rbl_avg
		each entry contains row addres of each bank and thread
	*/
		rule calc_rbl_matrix(calc_rbl_matrix_flag == 0);		//condition				
			if(rbl_address_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] == buffer_1.physical_address[30:16] )
			begin
				rbl_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] <=
				rbl_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]] + 1;
				
				row_hit_order[buffer_1.thread_id][row_hit_order_idx[buffer_1.thread_id] ] 
					<= last_access_req_id[buffer_1.thread_id][buffer_1.physical_address[15:13]];
			
				row_hit_order[buffer_1.thread_id][row_hit_order_idx[buffer_1.thread_id] + 1 ] 
					<= request_buffer_matrix_idx[buffer_1.thread_id]; 
					
				row_hit_order_idx[buffer_1.thread_id] <= row_hit_order_idx[buffer_1.thread_id] + 2;
				
				rbl_avg[buffer_1.thread_id] <= rbl_avg[buffer_1.thread_id] + 1;
			end
				
			rbl_address_matrix[buffer_1.thread_id][buffer_1.physical_address[15:13]]
				<= buffer_1.physical_address[30:16];
				
			last_access_req_id[buffer_1.thread_id][buffer_1.physical_address[15:13]]
				<= request_buffer_matrix_idx[buffer_1.thread_id];
				
			calc_rbl_matrix_flag <= 1;
		endrule : calc_rbl_matrix
		
	/*
		triggers clustering and matrix filling parallely
		
	*/
		rule fill_req(buffer_fifo.notEmpty() && calc_rbl_matrix_flag == 1 && calc_blp_matrix_flag == 1 && thread_request_flag == 1);
			buffer_1 <= buffer_fifo.first();
			buffer_fifo.deq();		
			calc_rbl_matrix_flag <= 0;
			calc_blp_matrix_flag <= 0;
			thread_request_flag <=0;
		endrule : fill_req
		
		
		rule calc_rbl_avg;		//condition
			for(int i = 0; i<8; i = i + 1)
			begin
				//let x = total_thread_request[i];
				rbl_avg[i] <= rbl_avg[i]/total_thread_request[i];
			end
		endrule : calc_rbl_avg
		
	/*
		used for initializtion for each quantum
	*/
		rule init (cnt == 0);		//condition starting of each quantum
			sumBW <= 0;
			totalBW <= 0;
			min_flag <= 1;
			thead_id_flag <= 0;
			mpki_flag <= 1;
			bwUsage_flag <= 1;
			thread_count <= 0;
			thread_request_flag <= 1;
			calc_blp_matrix_flag <= 1;
			calc_rbl_matrix_flag <=1;
			
			for(int i =0; i<8; i = i+1)
			begin
				visited_mpki_1[i] <= 0;
				blp_counter[i] <= 0;
				total_thread_request[i] <=0;
				//rbl_counter[i] <= 0;
				bandwidth_cluster[i] <= 1;
				for(int j = 0; j<8; j = j+1)
				begin
					blp_matrix[i][j] <= 0;
					rbl_address_matrix[i][j] <=0;
					rbl_matrix[i][j] <= 0;
				end
			end
			//quanta_flag <= 1;
		endrule : init			
		
		rule quanta(cnt <= 100000);
			cnt <= cnt + 1;
		endrule : quanta
		
/*--------------------INTERFACE METHODS-----------------------*/		
	
		method Action read_MPKI( mpki ) if(cnt == 100000); 	// quantum end condition
			for(int i = 0 ; i<8; i = i+1)
				mpki_1[i] <= mpki[i];
			//mpki_flag <= 1;
			cnt <= 0;		//1st cycle check 
		endmethod : read_MPKI
		
		method Action read_BW_usage( bwUsage )if(cnt == 100000);		// quantum end condition
			Bit#(17) lv_totalBW = 0;
			for(int i = 0; i<8; i = i+1)
			begin
				bwUsage_1[i] <= bwUsage[i];
				lv_totalBW = lv_totalBW + bwUsage_1[i];
			end
			totalBW <= lv_totalBW;
			//bwUsage_flag <= 1;
		endmethod : read_BW_usage
		
		method Action read_request(x);
			buffer_fifo.enq(x);
		endmethod : read_request
		
	endmodule : dram_scheduler

/* --------------- TEST BENCH ------------------------*/

(* synthesize *)
	module mkTb(Empty);
		
		Vector#(8,Reg#(Bit#(10))) mpki_tb <- replicateM( mkReg(0) );
		Vector#(8,Reg#(Bit#(17))) bwUsage_tb <- replicateM( mkReg(0) );
		
		Tcm_cluster_ifc ifc1 <- dram_scheduler;
			
	endmodule : mkTb
		
endpackage : prog1
