  
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# Essa funcao criar um script em python para um "cidade" em determinada "data" em um periodo de tempo
# de "from" ate "until" a cada "every" minutos
# A paralelizao eh feita dividindo as origens em grupos de 180, para todos os destinos


# CRIAR SCRIPT PYTHON PARALELO ALL MODES! ---------------------------------

# Argumentos interessantes (mais informacoes http://dev.opentripplanner.org/apidoc/1.0.0/resource_PlannerResource.html):
# waitReluctance: How much worse is waiting for a transit vehicle than being on a transit vehicle, as a multiplier. (df = 1)
# walkReluctance: A multiplier for how bad walking is, compared to being in transit for equal lengths of time. (df = 2)
# transferPenalty: An additional penalty added to boardings after the first,  roughly equivalent to seconds

# cidade <- "for"
# data <- "2018-05-12"
# from <- 7
# until <- 8
# every <- 15
# time_threshold <- 7200
# max_walk_distance = 800

criar_script_python_paral_modes <- function(cidade, modo = "todos",
                                            data, 
                                            from, until, every,
                                            time_threshold = 7200, max_walk_distance = 800
                                            ) {
  
  
  ano <- substr(data, 1, 4)
  mes <- substr(data, 6, 7)
  dia <- substr(data, 9, 10)
  
  ano1 <- as.integer(ano)
  mes1 <- as.integer(mes)
  dia1 <- as.integer(dia)

  # COMEÇA AQUI O SCRIPT ----------------------------------------------------

  
  
  
  transit <- c(
  "from __future__ import print_function",
  "import os",
  "from org.opentripplanner.scripting.api import OtpsEntryPoint",
  "from time import sleep",
  "import threading",
  "import shutil",
  "import time",
  "import gc",
  "",
  "# INPUT ########################################################################################",
  "",
  "# max number of threads to use in parallel",
  "max_threads = 16",
  "",
  "# Trips",
  sprintf("fromm = %i             # departure time start", from),
  sprintf("until = %i            # departure time end", until),
  sprintf("every = %i            # frequency (every 30 minutes)", every),
  sprintf("time_threshold = %s # Max travel time in seconds | 1h = 3600 seconds , 2h = 7200 seconds", time_threshold),
  sprintf("max_walk_distance = %s # Max walk distance", max_walk_distance),
  "",
  "# date of trips",
  sprintf("year= %i", ano1),
  sprintf("month = %i", mes1),
  sprintf("day = %i", dia1),
  sprintf("city = '%s'", cidade), 
  "",
  "#################################################################################################",
  "",
  "# import garbage collector (celan RAM memory)",
  "gc.collect()",
  "",
  "# Start timing the code",
  "start_time = time.time()",
  "",
  "# THREADED VERSION OF OTP SCRIPT",
  "",
  "# Instantiate an OtpsEntryPoint",
  sprintf("otp = OtpsEntryPoint.fromArgs(['--graphs', 'graphs', '--router', '%s'])", cidade),
  "",
  "# Get the default router",
  sprintf("router = otp.getRouter('%s')", cidade),
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "###   Public Transport   Public Transport",   
  "###   Public Transport   Public Transport",   
  "###   Public Transport   Public Transport",
  "",
  "################################################################################################",
  "################################################################################################",
  "",
  "gc.collect()",
  "",
  "filename = 'points/points_09.csv'",
  "batch_size = 128",
  "files = []",
  "try:",
  "  os.mkdir('temp_data')",
  "except:",
  "  print('folder already exists, continuing')",
  "",
  "with open(filename, 'r') as fin:",
  "  header = fin.next()",
  "  count = 0",
  "  for line in fin:",
  "    if count % 128 == 0:",
  "      fname = 'temp_data/' + str(count) + '_temp_points.csv'",
  "      files.append(fname)",
  "      try:",
  "        fout.close()",
  "      except:",
  "        pass",
  "      fout = open(fname, 'w')",
  "      fout.write(header)",
  "    fout.write(line)",
  "    count += 1",
  "  fout.close()",
  "",
  "################################################################################################",
  "for h in range(fromm, until):",
  "  for m in range(0,60,every):",
  "",
  "",
  "",
  "    # make a list of jobs to do",
  "    jobs = []",
  "    for points in files:",
  "      jobs.append(points)",
  "",
  "",
  "    def do_the_stuff(p):",
  "",  
  "      # Read Points of Destination - The file points.csv contains the columns GEOID, X and Y [inside]",
  "      points = otp.loadCSVPopulation(p, 'Y', 'X')",
  "      dests = otp.loadCSVPopulation(filename, 'Y', 'X')",
  "",
  "      # Create a default request for a given time",
  "      req = otp.createRequest()",
  "      req.setDateTime(year, month, day, h, m, 00)",
  "      req.setMaxTimeSec(time_threshold)  # 1h = 3600 seconds , 2h = 7200 seconds",
  "      # 'WALK,TRANSIT,BUS,RAIL,SUBWAY'",
  "      req.setModes('WALK,TRANSIT,BUS,RAIL,SUBWAY,TRAM')",
  "      req.setClampInitialWait(0)",
  "",
  "      # Create a CSV output",
  "      matrixCsv = otp.createCSVOutput()",
  "      matrixCsv.setHeader(['city', 'year', 'depart_time', 'origin', 'destination', 'walk_distance', 'travel_time', 'boardings'])",
  "",
  "      # Start Loop",
  "      for origin in points:",
  "        print(\"Processing origin: \", str(h) + \"-\" + str(m), \" \", origin.getStringData('id_hex'), 'on ', threading.current_thread())",
  "        req.setOrigin(origin)",
  "        spt = router.plan(req)",
  "        if spt is None:",
  "          continue",
  "        # Evaluate the SPT for all points",
  "        result = spt.eval(dests)",
  "        # Add a new row of result in the CSV output",
  "        for r in result:",
  "          matrixCsv.addRow([city, 'transit', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance(), r.getTime(), r.getBoardings()])",
  "",
  "      # Save the result",
  sprintf("      matrixCsv.save('../data/output_ttmatrix/ttmatrix_%s_pt_'+ str(h)+\"-\"+str(m) + \"_\" + p.split('/')[1])", cidade),
  "      gc.collect()",
  "",
  "",
  "# ^ that ^ function has to be defined before it's called",
  "# the threading bit is down here vvv",
  "#",
  "",
  "# how many threads do you want?",
  "#max_threads = int(raw_input('max threads (int) ? --> '))",
  "# start looping over jobs",
  "    threads = []",
  "    while len(jobs) > 0:",
  "      if threading.active_count() < max_threads + 1:",
  "        p = jobs.pop()",
  "        print(p)",
  "        thread = threading.Thread(target=do_the_stuff, args=(p,))",
  "        #thread.daemon = True",
  "        thread.start()",
  "        threads.append(thread)",
  "      else:",
  "        sleep(0.1)",
  "    # now wait for all daemon threads to end before letting",
  "    # the main thread die. Otherwise stuff will get cut off",
  "    # before it's finished",
  "    while threading.active_count() > 1:",
  "      sleep(0.1)",
  "    print('ALL JOBS COMPLETED!')",
  "    for t in threads:",
  "      t.join()",
  "",
  "    print(\"Elapsed time was %g seconds\" % (time.time() - start_time))",
  "",
  "",
  "shutil.rmtree('temp_data')")
  
  
  active <- c(
  "#################################################################################################",
  "#################################################################################################",
  "#################################################################################################",
  "",
  "print('ALL JOBS COMPLETED!')",
  "",
  "print(\"Elapsed time was %g seconds\" %
        (time.time() - start_time), \"using %g threads\" % (max_threads))",
  "",
  "",
  "",
  "",
  "",
  "",
  "###############################################################################################",
  "##### SINGLE DEPARTURE TIME #####",
  "###############################################################################################",
  "###############################################################################################",
  "###############################################################################################",
  "###############################################################################################",
  "",
  "gc.collect()",
  "",
  "",
  "# Read Points of Destination - The file points.csv contains the columns GEOID, X and Y.",
  sprintf("points = otp.loadCSVPopulation('points_corrigidos/points_corrigido_%s_09.csv', 'Y', 'X')", cidade),
  sprintf("dests = otp.loadCSVPopulation('points_corrigidos/points_corrigido_%s_09.csv', 'Y', 'X')", cidade),
  "",
  "",
  "",
  "",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "",
  "",
  "",
  "for h in range(10, 11):",
  "  for m in range(0,60, 60):",
  "",
  "    # Create a default request for a given time",
  "    req = otp.createRequest()",
  "    req.setDateTime(year, month, day, h, m, 00)",
  "    req.setMaxTimeSec(time_threshold)", 
  "    req.setModes('WALK')",
  "",
  "",
  "    # Create a CSV output",
  "    matrixCsv = otp.createCSVOutput()",
  "    matrixCsv.setHeader([ 'city', 'mode', 'depart_time', 'origin', 'destination', 'distance', 'travel_time' ]) # travel_time in seconds",
  "",
  "    # Start Loop",
  "    for origin in points:",
  "      print \"Processing walk\", str(h)+\"-\"+str(m),\" \", origin.getStringData('id_hex')",
  "      req.setOrigin(origin)",
  "      spt = router.plan(req)",
  "      if spt is None: continue",
  "",
  "      # Evaluate the SPT for all points",
  "      result = spt.eval(dests)",
  "",
  "      # Add a new row of result in the CSV output",
  "      for r in result:",
  "        matrixCsv.addRow([ city, 'walk', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance() , r.getTime()])",
  "",
  "    # Save the result",
  sprintf("    matrixCsv.save('../data/output_ttmatrix/ttmatrix_%s_walk.csv')", cidade),
  "    gc.collect()",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "",
  "",
  "",
  "for h in range(10, 11):",
  "  for m in range(0,60, 60):",
  "",
  "    # Create a default request for a given time",
  "    req = otp.createRequest()",
  "    req.setDateTime(year, month, day, h, m, 00)",
  "    req.setMaxTimeSec(time_threshold)", 
  "    req.setModes('BICYCLE')",
  "",
  "",
  "    # Create a CSV output",
  "    matrixCsv = otp.createCSVOutput()",
  "    matrixCsv.setHeader([ 'city', 'mode', 'depart_time', 'origin', 'destination', 'distance', 'travel_time' ]) # travel_time in seconds",
  "",
  "    # Start Loop",
  "    for origin in points:",
  "      print \"Processing bike\", str(h)+\"-\"+str(m),\" \", origin.getStringData('id_hex')",
  "      req.setOrigin(origin)",
  "      spt = router.plan(req)",
  "      if spt is None: continue",
  "",
  "      # Evaluate the SPT for all points",
  "      result = spt.eval(dests)",
  "",
  "      # Add a new row of result in the CSV output",
  "      for r in result:",
  "        matrixCsv.addRow([ city, 'bike', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance() , r.getTime()])",
  "",
  "    # Save the result",
  sprintf("    matrixCsv.save('../data/output_ttmatrix/ttmatrix_%s_bike.csv')", cidade),
  "    gc.collect()",
  "",
  "",
  "",
  "#################################################",
  "",
  "# Stop timing the code",
  "print 'ALL JOBS COMPLETED!'",
  "print(\"Elapsed time was %g seconds\" % (time.time() - start_time))")
  
  # Start writing to an output file
  output_file <- sprintf("../otp/py/otp_%s_%s-%s.py", cidade, ano, mes)
  
  sink(output_file)

  if (modo == "tp") {

    cat(transit, sep = "\n")

  } else if (modo == "ativo") {

    cat(active, sep = "\n")

  } else if (modo == "todos") {

    cat(transit, active, sep = "\n")

  }
  
  # Stop writing to the file
  sink()
  
    
}


# criar_script_python_paral_modes("for", modo = "todos", "2018-12-05", from = 7, until = 8, every = 15)

