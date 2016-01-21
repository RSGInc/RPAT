var running = 0;

$(document).ajaxStop($.unblockUI); 
            var stateFileDictionary = {
            "accident_rates.csv":{"description":"Accident Rates"},
            "fuel.csv":{"description":"Fuel"},
            "fuel_co2.csv":{"description":"Emissions Rate"},
            "keyvalue.csv":{"description":"Individual Model Parameters"},
            "place_type_elasticities.csv":{"description":"Place Type Adjustments to Travel Demand: Elasticities"},
            "place_type_relative_values.csv":{"description":"Place Type Adjustments to Travel Demand"},
            "tdm_ridesharing.csv":{"description":"Travel Demand Management: Ridesharing"},
            "tdm_transit.csv":{"description":"Travel Demand Management: Transit Fare"},
            "tdm_transitlevels.csv":{"description":"Travel Demand Management: Transit Fare Levels"},
            "tdm_vanpooling.csv":{"description":"Travel Demand Management: Vanpooling"},
            "tdm_workschedule.csv":{"description":"Travel Demand Management: Work Schedule"},
            "tdm_workschedulelevels.csv":{"description":"Travel Demand Management: Work Schedule Levels"},
            "transportation_costs.csv":{"description":"Transportation Costs"},
            "vehicle_mpg.csv":{"description":"Vehicle Fuel Economy"}
            };
			var outputDir;
			
            var outputFileDictionary = {
                "Dvmt.Pt.csv":{"category":"directTravelImpacts", "description":"Daily Vehicle Miles Traveled", "usage":""},
                "AveSpeed.MaTy.csv":{"category":"directTravelImpacts","description":"Average Travel Speeds by Vehicle Type", "usage":""},
                "Costs.Pt.csv":{"category":"financialAndEconomicImpacts","description":"Annual Traveler Cost (Fuel + Charges)", "usage":""},                
                "Equity.Ig.csv":{"category":"communityImpacts","description":"Job Accessibility by Income Group", "usage":""},
                "TransitCapCost.Ma.csv":{"category":"financialAndEconomicImpacts","description":"Regional Transit Infrastructure Cost", "usage":""},
                "VehHr.MaTy.csv":{"category":"directTravelImpacts","description":"Vehicle Hours of Travel", "usage":""},
                "Emissions.Pt.csv":{"category":"environmentAndEnergyImpacts","description":"Greenhouse Gas Emissions", "usage":""},
                "Fuel.Pt.csv":{"category":"environmentAndEnergyImpacts","description":"Fuel Consumption", "usage":""},
                "DelayVehHr.MaTy.csv":{"category":"directTravelImpacts","description":"Vehicle Hours of Delay", "usage":""},
                "HighwayCost.Ma.csv":{"category":"financialAndEconomicImpacts","description":"Regional Highway Infrastructure Costs", "usage":""},
                "Inc.Pt.csv":{"category":"summariesOfInputs","description":"Income by Place Type", "usage":""},
                "Pop.Pt.csv":{"category":"summariesOfInputs","description":"Population by Place Type", "usage":""},
                "Emp.Pt.csv":{"category":"summariesOfInputs","description":"Employment by Place Type", "usage":""},
                "Access.Ma.csv":{"category":"locationImpacts","description":"Regional Accessibility", "usage":""},
                "Accidents.As.csv":{"category":"communityImpacts","description":"Accident Rates", "usage":""},
                "TransitOpCost.Ma.csv":{"category":"financialAndEconomicImpacts","description":"Regional Transit Operating Cost", "usage":""},
                "TransitTrips.Pt.csv":{"category":"directTravelImpacts","description":"Daily Transit Trips", "usage":""},
                "VehicleTrips.Pt.csv":{"category":"directTravelImpacts","description":"Daily Vehicle Trips", "usage":""},
                "Walking.Ma.csv":{"category":"communityImpacts","description":"Walking Percentage Increase", "usage":""}
            };
            
            var categoryDictionary = {
				"1":{"name":"Built Environment", "div":"build_environment"},
				"2":{"name":"Demand", "div":"demand"},
				"3":{"name":"Policy","div":"policy"},
				"4":{"name":"Supply","div":"supply"},
				"5":{"name":"Settings", "div":"settingsFiles"}
			}
            
            var fileDictionary = {
                "auto_op_cost_growth.csv": {"category":"3", "description":"% Increase in Auto Operating Cost", "long_name":"% Increase in Auto Operating Cost (auto_op_cost_growth.csv)"},
                "base_vmt.csv": {"category":"2", "description":"Base Daily Vehicle Miles Traveled", "long_name":"Base Daily Vehicle Miles Traveled (base_vmt.csv)"},
                "commute_options.csv": {"category":"3", "description":"% of Employees Offerred Commute Options", "long_name":"% of Employees Offered Commute Options (commute_options.csv)"},
                "employment.csv": {"category":"2", "description":"Employment (Existing)", "long_name":"Employment (Existing) (employment.csv)"},
                "employment_growth.csv": {"category":"2", "description":"Employment (Growth)", "long_name":"Employment (Growth) (employment_growth.csv) "},
                "its.csv": {"category":"3", "description":"% Road Miles with ITS Treatment", "long_name":"% Road Miles with ITS Treatment (its.csv)"},
                "light_vehicles.csv": {"category":"3", "description":"Bicycling/Light Vehicle Targets", "long_name":"Bicycling/Light Vehicle Targets (light_vehicles.csv)"},
                "parking_growth.csv": {"category":"3", "description":"Increase in Parking Cost and Supply", "long_name":"Increase in Parking Cost and Supply (parking_growth.csv)"},
                "place_type_existing.csv": {"category":"1", "description":"Population and Jobs by Place Type", "long_name":"Population and Jobs by Place Type (Place_type_existing.csv)"},
                "place_type_growth.csv": {"category":"3", "description":"% Growth by Place Type", "long_name":"% Growth by Place Type (place_type_growth.csv)"},
                "pop_by_age.csv": {"category":"2", "description":"Population (Existing and Growth)", "long_name":"Population (Existing and Growth) (pop_by_age.csv)"},
                "regional_income.csv": {"category":"2", "description":"Regional Income", "long_name":"Regional Income (regional_income.csv)"},
                "transportation_supply.csv": {"category":"4", "description":"Road Lane Miles and Transit Revenue Miles", "long_name":"Road Lane Miles and Transit Revenue Miles (transportation_supply.csv)"},
                "transportation_supply_growth.csv": {"category":"3", "description":"% Increase in Road Lane Miles, Transit Revenue Miles per Cap.", "long_name":"% Increase in Road Lane Miles, Transit Revenue Miles per Cap. (transportation_supply_growth.csv)"},
                "trips_per_cap.csv": {"category":"2", "description":"Auto and Transit Trips per Capita", "long_name":"Auto and Transit Trips per Capita (trips_per_cap.csv)"},
                "truck_bus_vmt.csv": {"category":"2", "description":"Truck and Bus Vehicle Miles Traveled", "long_name":"Truck and Bus Vehicle Miles Traveled (truck_bus_vmt.csv)"},
                "vmt_charge.csv": {"category":"3", "description":"Auto Operating Surcharge per VMT", "long_name":"Auto Operating Surcharge per VMT (vmt_charge.csv)"}
            }
            
			var runid;
			var $container,$outputContainer, $stateContainer, statehandsontable,handsontable, cols, fileName, stateFileName, stateDirectory;
			
			function showFiles(){
				$("#documentationdiv").css("display", "none");
			}
			
			function loadOutputDirectories(){
				$("#outputDirectory").html("");
				$("#outputDirectories li").remove();
				$.ajax({
					url:"output_directories",
					dataType:"json",
					data:{name:$("#manageScenario").val()},
					success: function(data){
						$.each(data.files, function(index, value){
							if(value.indexOf(".csv")>-1){
								$("#outputDirectories").append("<li onclick='return loadOutputFiles(\""+value+"\", this)'>"+value+"</li>");
							}
						});
						outputDir = data.directory;
						$("#outputDirectory").html("<a href='#' onclick=\"openOutputDirectory()\" >Open Output Directory</a>");
					},
					cache:false
				});
			}
			
			function openOutputDirectory(location){
				$.ajax({
					url:"open_output_directory",
					dataType:"json",
					data:{name:outputDir},
					cache:false,
					success: function(data){
						
					}
					
				});
				
			}
			
			function loadOutputFiles(){
				
				$("#outputDirectories li").css('font-weight', 'normal');
				$("#outputDirectories li").css('text-decoration', 'none');
				
                $("#communityImpactFiles li").remove();
                $("#directTravelImpactFiles li").remove();
                $("#environmentAndEnergyImpactFiles li").remove();
                $("#financialAndEconomicImpactFiles li").remove();
                $("#locationImpactFiles li").remove();
                $("#summariesOfInputFiles li").remove();
                
				$.ajax({
					url:"output_files",
					dataType:"json",
					data:{name:$("#manageScenario").val()},
					cache:false,
					success: function(data){
						$.each(data.files, function(index, value){
							if(value.indexOf(".csv")>1){
								if(outputFileDictionary[value].category == "communityImpacts"){
									$("#communityImpactFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");	
								}else if(outputFileDictionary[value].category == "directTravelImpacts"){
									$("#directTravelImpactFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");
								}else if(outputFileDictionary[value].category == "environmentAndEnergyImpacts"){
									$("#environmentAndEnergyImpactFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");
								}else if(outputFileDictionary[value].category == "financialAndEconomicImpacts"){
									$("#financialAndEconomicImpactFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");
								}else if(outputFileDictionary[value].category == "locationImpacts"){
									$("#locationImpactFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");
								}else if(outputFileDictionary[value].category == "summariesOfInputs"){
									$("#summariesOfInputFiles").append("<li onclick='return loadOutputFile(\""+value+"\", this)'>"+outputFileDictionary[value].description+"</li>");
								}
							}
						});
						$("#outputFileDisplay").css("display", "block");
						
					}
				});
			}
			
			function loadOutputFile(fileName, linkReference){
            
                $("#communityImpactFiles li").css('font-weight', 'normal');
				$("#directTravelImpactFiles li").css('font-weight', 'normal');
				$("#environmentAndEnergyImpactFiles li").css('font-weight', 'normal');
				$("#financialAndEconomicImpactFiles li").css('font-weight', 'normal');
				$("#locationImpactFiles li").css('font-weight', 'normal');
				$("#summariesOfInputFiles li").css('font-weight', 'normal');
                
                $("#communityImpactFiles li").css('text-decoration', 'none');
				$("#directTravelImpactFiles li").css('text-decoration', 'none');
				$("#environmentAndEnergyImpactFiles li").css('text-decoration', 'none');
				$("#financialAndEconomicImpactFiles li").css('text-decoration', 'none');
				$("#locationImpactFiles li").css('text-decoration', 'none');
				$("#summariesOfInputFiles li").css('text-decoration', 'none');
             
				$(linkReference).css('text-decoration', 'underline');
				$(linkReference).css('font-weight', 'bold');
				
				if($outputContainer != null){
					$outputContainer.handsontable('destroy');
				}
				
				$outputContainer = $("#outputTable");
				$("#outputdocumentationdiv").css("display", "block");
				var outputDocumentation = "<h4>"+outputFileDictionary[fileName].description+"</h4>";
				outputDocumentation += "<h5>Description</h5>" + fileName;
				if(outputFileDictionary[fileName].usage != ''){
					outputDocumentation +=  ": "+outputFileDictionary[fileName].usage;
				}
                 $("#outputdocumentationnamediv").html("<h4>"+outputFileDictionary[fileName].description + "("+ fileName +")</h4>");
                $("#outputdocumentationdiv").html('');
                
                $.ajax({
				    url:"load_documentation",
					data:{filename: fileName.replace('csv', 'html') },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      $("#outputdocumentationdiv").html(res.data);
				    }
				  });
                
                $.ajax({
				    url:"load_documentation",
					data:{filename: fileName.replace('csv', 'html') },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      
				    }
				  });
                
				$("#outputDocumentationDisplay").css("display", "block")
				$.ajax({
				    url:"loadoutputcsvfile",
					data:{name:$("#manageScenario").val(), fileName:fileName },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      cols = [];
				      $.each(res.data[0], function(index, value){
				      	cols.push(value);
				      });
					$("#outputTableDisplay").css("display", "block");
				     table = $outputContainer.handsontable({
						  startRows: res.data.length,
						  startCols: cols.length,
						  rowHeaders: true,
						  colHeaders: true,
						  minSpareRows: 0,
						  contextMenu: false,
						  onChange: function (change, source) {
						    if (source === 'loadData') {
						      return; //don't save this change
						    }
						    
						  }
						});
						handsontable = $outputContainer.data('handsontable');
						handsontable.loadData(res.data);
				    }
				  });
			}
			
			function loadFile(fileNameParameter, linkItem, fileType){
				$("#build_environment li").css('text-decoration', 'none');
				$("#demand li").css('text-decoration', 'none');
				$("#policy li").css('text-decoration', 'none');
				$("#supply li").css('text-decoration', 'none');
				$("#settingsFiles li").css('text-decoration', 'none');
				$("#stateFiles li").css('text-decoration', 'none');
				$("#stateForecastFiles li").css('text-decoration', 'none');
				
				$("#build_environment li").css('font-weight', 'normal');
				$("#demand li").css('font-weight', 'normal');
				$("#policy li").css('font-weight', 'normal');
				$("#supply li").css('font-weight', 'normal');
				$("#settingsFiles li").css('font-weight', 'normal');
				$("#stateFiles li").css('font-weight', 'normal');
				$("#stateForecastFiles li").css('font-weight', 'normal');
				$(linkItem).css('text-decoration', 'underline');
				$(linkItem).css('font-weight', 'bold');
				$("#documentationdiv").css("display", "block");
				fileName = fileNameParameter;
				$("#documentationnamediv").html("<h4>"+fileDictionary[fileName].long_name + "</h4>");
				
				if($container != null){
					$container.handsontable('destroy');
				}
				$container = $("#editTable2");
				
                $.ajax({
				    url:"load_documentation",
					data:{filename: fileNameParameter.replace('csv', 'html') },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      $("#documentationdiv").html(res.data);
                      $("#innertest").collapse('show');
				    }
				  });
                
				$.ajax({
				    url:fileType=="text"?"loadTextFile":"loadcsvfile",
					data:{name:$("#manageScenario").val(), fileName:fileName },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      cols = [];
				      $.each(res.data[0], function(index, value){
				      	cols.push(value);
				      });
				     $("#buttondiv").css("display", "block");
				     $("#stateFileEdit").css("display", "none");
				     table = $container.handsontable({
						  startRows: res.data.length,
						  startCols: cols.length,
						  rowHeaders: true,
						  colHeaders: true,
						  colWidths:fileType=="text"?[400]:[200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200],
						  //colHeaders: cols,
						  minSpareRows: 0,
						  contextMenu: false,
						  onChange: function (change, source) {
						    if (source === 'loadData') {
						      return; //don't save this change
						    }
						    if ($parent.find('input[name=autosave]').is(':checked')) {
						      clearTimeout(autosaveNotification);
						      $.ajax({
						        url: "json/save.json",
						        dataType: "json",
						        type: "POST",
						        data: res,
						        cache:false,
						        //data: change, //contains changed cells' data
						        complete: function (data) {
						          $console.text('Autosaved (' + change.length + ' cell' + (change.length > 1 ? 's' : '') + ')');
						          autosaveNotification = setTimeout(function () {
						            $console.text('Changes will be autosaved');
						          }, 1000);
						        }
						      });
						    }
						  }
						});
						handsontable = $container.data('handsontable');
						handsontable.loadData(res.data);
				    }
				  });
				return false;
			}
			
			function loadStateFile(fileName, linkItem, directory, fileType){
				
				$("#build_environment li").css('text-decoration', 'none');
				$("#demand li").css('text-decoration', 'none');
				$("#policy li").css('text-decoration', 'none');
				$("#supply li").css('text-decoration', 'none');
				$("#settingsFiles li").css('text-decoration', 'none');
				$("#stateFiles li").css('text-decoration', 'none');
				$("#stateForecastFiles li").css('text-decoration', 'none');
				
				$("#build_environment li").css('font-weight', 'normal');
				$("#demand li").css('font-weight', 'normal');
				$("#policy li").css('font-weight', 'normal');
				$("#supply li").css('font-weight', 'normal');
				$("#settingsFiles li").css('font-weight', 'normal');
				$("#stateFiles li").css('font-weight', 'normal');
				$("#stateForecastFiles li").css('font-weight', 'normal');
				$(linkItem).css('text-decoration', 'underline');
				$(linkItem).css('font-weight', 'bold');
				stateFileName = fileName;
				stateDirectory = directory;
				if($stateContainer != null){
					$stateContainer.handsontable('destroy');
				}
				
				$("#statedocumentationdiv").css("display", "block");
				if(stateFileDictionary[stateFileName] != null){

				}else{
					if(fileName.indexOf("pop_by_age")>-1){
						var text = "Population estimates/forecasts by county and age cohort for "+fileName.replace('pop_by_age_', '').replace('.csv', '');
						$("#statedocumentationdiv").html("<h4>"+text + "</h4>" + stateFileDictionary["pop_by_age"].usage);
					}else{
						$("#statedocumentationdiv").css("display", "none");
					}
				}
                
                $("#parameter_documentation_name_div").html("<h4>"+stateFileDictionary[stateFileName].description+" ("+fileName+")</h4>");
                $("#statedocumentationdiv").html("");
                $.ajax({
				    url:"load_documentation",
					data:{filename: fileName.replace('csv', 'html') },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      $("#statedocumentationdiv").html(res.data);
				    }
				  });
                
				$stateContainer = $("#editStateTable2");
				$.ajax({
					url:fileType=="text"?"loadstatetextfile":"loadstatecsvfile",
					data:{name:$("#manageScenario").val(), fileName:stateFileName, directory:directory },
				    dataType: 'json',
				    type: 'GET',
				    cache:false,
				    success: function (res) {
				      cols = [];
				      $.each(res.data[0], function(index, value){
				      	cols.push(value);
				      });
				     $("#stateFileEdit").css("display", "block");
				     $("#buttondiv").css("display", "none");
				     table = $stateContainer.handsontable({
						  startRows: res.data.length,
						  startCols: cols.length,
						  rowHeaders: true,
						  colHeaders: true,
						  minSpareRows: 0,
						  colWidths:fileType=="text"?[400]:[100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100],
						  contextMenu: false,
						  onChange: function (change, source) {
						    if (source === 'loadData') {
						      return;
						    }
						  }
						});
						statehandsontable = $stateContainer.data('handsontable');
						statehandsontable.loadData(res.data);
				    }
				  });
				return false;
			}
			
			function deleteScenario(scenarioName){
				$("#directoryToDelete").val(scenarioName);
				$("#deleteConfirm").modal('show');
			}
			
			function loadScenarioes(){
				$("#scenarios li").remove();
				$("#manageScenario option").remove();
				$("#manageScenario").append('<option>Please select</option>')
				
                $("#report_scenarios").html('');
                
				$("#firstCopyScenario option").remove();
				loadScenariosToCopy();
				$.ajax({
					url:"scenarioes",
					dataType:"json",
					cache:false,
					success: function(data){
						if(data.scenarios.length>0){
							$("#firstRun").css("display", "none");
							$("#alreadySetup").css("display", "block");
							$("#NewScenario2").css("display", "inline");
						}else{
							$("#firstRun").css("display", "block");
							$("#alreadySetup").css("display", "none");
							$("#NewScenario2").css("display", "none");
						}
						$.each(data.scenarios, function(index, value){
							$("#scenarios").append("<li><div class=scenario >"+value+"<div style='float:right;'><button  class='btn' onclick='deleteScenario(\""+value+"\")' >Delete</button></div></div></li>");
							$("#manageScenario").append("<option>"+value+"</option>");
                            
                            $("#report_scenarios").append("<input type=checkbox name='report_scenario[]' value=\""+value+"\"> "+value + "<br />");
                            
						});
					}
				});
			}
			function loadScenariosToCopy(){
				$.ajax({
					url:"scenarioes_to_copy",
					dataType:"json",
					cache:false,
					success: function(data){
						$("#copyScenario option").remove();
						$.each(data.root_scenarios, function(index, value){
							$("#copyScenario").append("<option value='"+value+"'>"+value+"</option>");
							$("#firstCopyScenario").append("<option value='"+value+"'>"+value+"</option>");
						});
						$.each(data.scenarios, function(index, value){
							$("#copyScenario").append("<option value='"+value+"'>"+value+"</option>");
							$("#firstCopyScenario").append("<option value='"+value+"'>"+value+"</option>");
						});
					}
				});
			}
			
            var model_step = {
                "pending":{"order":0},
                "household":{"order":1},
                "urban":{"order":2},
                "accessibility":{"order":3},
                "vehicle":{"order":4},
                "demand":{"order":5},
                "congestion":{"order":6},
                "induced":{"order":7},
                "policyvmt":{"order":8},
                "policycongestion":{"order":9},
                "metrics":{"order":10},
                "done":{"order":11}
                    };
            
            
			function runStatus(){
				$.ajax({
					url:"runstatus",
					dataType:"json",
					cache:false,
					success: function(data){
                        $.each(model_step, function(index, value){
                            $("#"+index+"_step").css("background-color", model_step[data.output.replace("\n", "")].order > value.order?"green":"lightgray");
                            if(model_step[data.output.replace("\n", "")].order === value.order){
                                $("#"+index+"_step").css("background-color", model_step[data.output.replace("\n", "")].order > value.order?"green":"lightgray");
                            }
                        });
                        if(data.output.replace("\n", "") === "done" && running === 1){
                            $("#runningScenario").html($("#manageScenario").val() +" scenario done running");
                            loadOutputFiles();
                            running = 0;
                        }
					}
				});
			}
		
			window.setInterval(function(){
				runStatus()
			}, 5000);
			$(document).ready(function(){
            
                $('#doc_accordion .collapse').on('shown.bs.collapse', function(){
                    $(this).parent().find("#showhidetext").html('Hide ');
                }).on('hidden.bs.collapse', function(){
                    $(this).parent().find("#showhidetext").html('Show ');
                });
                
                $('#state_doc_accordion .collapse').on('shown.bs.collapse', function(){
                    $(this).parent().find("#showhidestatetext").html('Hide ');
                }).on('hidden.bs.collapse', function(){
                    $(this).parent().find("#showhidestatetext").html('Show ');
                });
                
                $('#output_doc_accordion .collapse').on('shown.bs.collapse', function(){
                    $(this).parent().find("#showhideoutputtext").html('Hide ');
                }).on('hidden.bs.collapse', function(){
                    $(this).parent().find("#showhideoutputtext").html('Show ');
                });
            
				//outputFileAccordion
				$("#outputFileAccordion").on("hide", function(event){
					$("#outputTableDisplay").css("display", "none");
					$("#outputDirectories li").css('font-weight', 'normal');
					$("#outputDirectories li").css('text-decoration', 'none');
					
					$("#economyOutputFiles li").css('font-weight', 'normal');
				$("#fuelOutputFiles li").css('font-weight', 'normal');
				$("#populatoinOutputFiles li").css('font-weight', 'normal');
				$("#transportationOutputFiles li").css('font-weight', 'normal');
				$("#travelOutputFiles li").css('font-weight', 'normal');
				$("#vehicleOutputFiles li").css('font-weight', 'normal');
				
				$("#economyOutputFiles li").css('text-decoration', 'none');
				$("#fuelOutputFiles li").css('text-decoration', 'none');
				$("#populatoinOutputFiles li").css('text-decoration', 'none');
				$("#transportationOutputFiles li").css('text-decoration', 'none');
				$("#travelOutputFiles li").css('text-decoration', 'none');
				$("#vehicleOutputFiles li").css('text-decoration', 'none');
				});
				
				$("#stateaccordian").on("hide", function(event){
					$("#build_environment li").css('text-decoration', 'none');
					$("#demand li").css('text-decoration', 'none');
					$("#policy li").css('text-decoration', 'none');
					$("#supply li").css('text-decoration', 'none');
					$("#settingsFiles li").css('text-decoration', 'none');
					$("#stateFiles li").css('text-decoration', 'none');
					$("#stateForecastFiles li").css('text-decoration', 'none');
					$("#build_environment li").css('font-weight', 'normal');
					$("#demand li").css('font-weight', 'normal');
					$("#policy li").css('font-weight', 'normal');
					$("#supply li").css('font-weight', 'normal');
					$("#settingsFiles li").css('font-weight', 'normal');
					$("#stateFiles li").css('font-weight', 'normal');
					$("#stateForecastFiles li").css('font-weight', 'normal');
					$("#documentationdiv").css("display", "block");
					$("#stateFileEdit").css("display", "none");
				});
				$("#accordion2").on("hide", function(event){
					$("#build_environment li").css('text-decoration', 'none');
					$("#demand li").css('text-decoration', 'none');
					$("#policy li").css('text-decoration', 'none');
					$("#supply li").css('text-decoration', 'none');
					$("#settingsFiles li").css('text-decoration', 'none');
					$("#stateFiles li").css('text-decoration', 'none');
					$("#stateForecastFiles li").css('text-decoration', 'none');
					$("#build_environment li").css('font-weight', 'normal');
					$("#demand li").css('font-weight', 'normal');
					$("#policy li").css('font-weight', 'normal');
					$("#supply li").css('font-weight', 'normal');
					$("#settingsFiles li").css('font-weight', 'normal');
					$("#stateFiles li").css('font-weight', 'normal');
					$("#stateForecastFiles li").css('font-weight', 'normal');
					$("#documentationdiv").css("display", "block");
					$("#buttondiv").css("display", "none");
				});
                
                $("#run_report").click(function(){
                    var data = { 'scenarios' : [], 'metrics':[], 'measures':[]};
                    $("input[name='report_scenario[]']:checked").each(function() {
                      data['scenarios'].push($(this).val());
                    });
                    $("input[name='report_metrics[]']:checked").each(function() {
                      data['metrics'].push($(this).val());
                    });
                    $("input[name='report_measure[]']:checked").each(function() {
                      data['measures'].push($(this).val());
                    });
                    $("#report_indicator").css("display", "block");
                   $.ajax({
					url:"runReport",
					dataType:"json",
					data:data,
					cache:false,
					success: function(data){
                        $.each(data.images, function(index, value){
                            $("#report_images").append("<img style='width:400px' width=400 style='padding:5px;' src='/reports/"+value+"' />");
                        });
                        $("#report_indicator").css("display", "none");
					}
				}); 
                });
                $("#clear_report").click(function(){
                    $("#report_images").html('');
                });
               
				$.ajax({
					url:"get_default_state",
					dataType:"json",
					data:{},
					cache:false,
					success: function(data){
						if(data.state != null){
							$("#state").val(data.state);
							$("#firstRunState").val(data.state);
							$("#statespan").html("("+$("#state option:selected").text()+")");
						}
					}
				});
				
				$("#modelStates").change(function(){
					$("#stateFiles li").remove();
					$.ajax({
						url:"state_files",
						dataType:"json",
						cache:false,
						data:{name:$("#modelStates").val()},
						success: function(data){
							$.each(data.files, function(index, value){
								if(value.indexOf('.csv') >1){
									if(stateFileDictionary[value] != null){
										$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+stateFileDictionary[value].description+"</li>");
									}else{
										$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+value+"</li>");
									}
								}
								if(value.indexOf('.txt')>1){
									$("#stateFiles li").eq(0).before("<li onclick='return loadStateFile(\""+value+"\", this, \"root\", \"text\")'>"+stateFileDictionary[value].description+"</li>");
								}
							});
						}
					});
				});
               
				$("#firstRunStateButton").click(function(){
					if($("#firstRunState").val() == -1){
						alert('Please select a state.')
					}else{
						$.ajax({
							url:"set_default_state",
							dataType:"json",
							data:{"state":$("#firstRunState").val()},
							cache:false,
							success: function(data){
								$("#state").val($("#firstRunState").val());
								$("#statespan").html("("+$("#state option:selected").text()+")");
								loadScenariosToCopy();
							}
						});
						$("#firstRunDiv").modal('hide');
					}
				});
				
				$("#startrun").click(function(){
                    running = 1;
					$("#runningScenario").html($("#manageScenario").val() +" scenario running <br /><img src='/img/progress.gif'></img>");
					$.ajax({
						url:"startrun",
						dataType:"json",
						data: {"name": $("#manageScenario").val()},
						cache:false,
						complete:function(){
							$("#startrun").css("display", "block");
							$("#runningScenario").html("");
						},
						success: function(data){		
							runid = data.pid;
                            loadOutputFiles();
						}
					});	
				});
          
				$("#save").click(function(){
					$.blockUI({ message: "Saving" });
					$.ajax({
		              url: fileName=="run_parameters.txt"?"saveTextFile":"savecsvfile",
		              data: {"data": JSON.stringify(handsontable.getData()), name:$("#manageScenario").val(), fileName:fileName},
		              dataType: 'json',
		              type: 'POST',
		              cache:false,
		              success: function (res) {
		              	$("#stateFileEdit").css("display", "none");
				     	$("#buttondiv").css("display", "none");
		              	$("#build_environment li").remove();
						$("#demand li").remove();
						$("#policy li").remove();
						$("#supply li").remove();
						$("#settingsFiles li").remove();
						$("#stateFiles li").remove();
						$("#stateForecastFiles li").remove();
						$.ajax({
							url:"state_files",
							dataType:"json",
							data:{name:$("#manageScenario").val()},
							cache:false,
							success: function(data){
								$.each(data.files, function(index, value){
									if(value.indexOf('.csv')>1){
										
										if(stateFileDictionary[value] != null){
											$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+stateFileDictionary[value].description+"</li>");
										}else{
											$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+value+"</li>");
										}
									}
								});
							}
						});
						
						//NEW
						$.ajax({
						url:"scenario",
						dataType:"json",
						data:{name:$("#manageScenario").val()},
						cache:false,
						success: function(data){
							$.each(data.files, function(index, value){
								var icon = "";
								if(data.file_edits[index]){
									icon = "<img src='/img/tick.png' />";
								}
								if(categoryDictionary[fileDictionary[value].category].name == "Settings"){
									$("#"+categoryDictionary[fileDictionary[value].category].div).append("<li onclick='return loadFile(\""+value+"\", this, \"text\")'> " +icon+ " "+fileDictionary[value].description+"</li>");
								}else{
									if(value=='phev_characteristics.csv'){
											$("#"+categoryDictionary[fileDictionary[value].category].div +" li").eq(7).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else if(value=='comm_service_fuel.csv'){	
											$("#"+categoryDictionary[fileDictionary[value].category].div + " li").eq(0).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else if(value=='other_ops.csv'){
											$("#"+categoryDictionary[fileDictionary[value].category].div + " li").eq(6).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else{
											
									$("#"+categoryDictionary[fileDictionary[value].category].div).append("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
									}
								}
							});
						}
					});
		              },
		              error: function () {
		              }
		            });
				});
				$("#saveStateFile").click(function(){
					$.blockUI({ message: "Saving" });
					$.ajax({
					  url: stateFileName=="global_values.txt"?"savestatetextfile":"savestatecsvfile",
		              data: {"data": JSON.stringify(statehandsontable.getData()), name:$("#manageScenario").val(), fileName:stateFileName, directory:stateDirectory},
		              dataType: 'json',
		              type: 'POST',
		              cache:false,
		              success: function (res) {
		              	$("#stateFileEdit").css("display", "none");
				     	$("#buttondiv").css("display", "none");
		              },
		              error: function () {
		              }
		            });
		          // }
				});
				
				loadScenarioes();
				$("#NewScenario").click(function(){
					$("#newScenarioForm").modal('show');
				});
				$("#NewScenario2").click(function(){
					$("#newScenarioForm").modal('show');
				});
				
				$("#setupbutton").click(function(){
					$("#getStarted").modal('show');
				});
					
				$("#deleteScenarioButton").click(function(){
					$.ajax({
						url:"delete_scenario",
						dataType:"json",
						data:{name:$("#directoryToDelete").val()},
						cache:false,
						success: function(data){
							
							loadScenarioes();
						}
					});
					$("#deleteConfirm").modal('hide');
				});	
					
				$("#createScenarioButton").click(function(){
					if($("#scenarioname").val() == '' ){
						$("#scenarioAlert").css("display", "block");
					}else{
						$("#scenarioAlert").css("display", "none");
						$.ajax({
							url:"new_scenario",
							dataType:"json",
							data:{
								name:$("#scenarioname").val(),
								fromScenario:$("#copyScenario").val(),
								isFirst:0
							},
							cache:false,
							success: function(data){
								$("#scenarioname").val('');
								loadScenarioes();
							}
						});
						$("#newScenarioForm").modal('hide');
					}
				});
				
				$("#firstCreateScenarioButton").click(function(){
                    $("#firstScenarioAlert").css("display", "none");
                    $("#stateAlert").css("display", "none");
                    $.ajax({
                        url:"new_scenario",
                        dataType:"json",
                        data:{
                            name:$("#firstScenarioName").val(),
                            fromScenario:'template_scenario',
                            isFirst:1
                        },
                        cache:false,
                        success: function(data){
                            $("#firstScenarioName").val('');
                            loadScenarioes();
                        }
                    });
                    $("#getStarted").modal('hide');
				});
				
				$("#manageScenario").change(function(){
                    if($("#manageScenario").val() != "-1"){
                        $("#scenario_details").css("display", "block");
                        
                        $.ajax({
                        url:"resetrunstatus",
                        dataType:"json",
                        data:{

                        },
                        cache:false,
                        success: function(data){

                        }
                    });
                    }else{
                        $("#scenario_details").css("display", "none");
                    }
					$("#build_environment li").remove();
					$("#demand li").remove();
					$("#policy li").remove();
					$("#supply li").remove();
					$("#settingsFiles li").remove();
					$("#stateFiles li").remove();
					$("#stateForecastFiles li").remove();
					$("#buttondiv").css("display", "none");
					$("#stateFileEdit").css("display", "none");
					$("#documentationdiv").css("display", "none");
                    loadOutputFiles();
					$.ajax({
						url:"state_files",
						dataType:"json",
						data:{name:$("#manageScenario").val()},
						cache:false,
						success: function(data){
							$.each(data.files, function(index, value){
								if(value.indexOf('.csv')>1){
									if(stateFileDictionary[value] != null){
										$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+stateFileDictionary[value].description+"</li>");
									}else{
										$("#stateFiles").append("<li onclick='return loadStateFile(\""+value+"\", this, \"root\")'>"+value+"</li>");
									}
								}
								if(value.indexOf('.txt')>1){
									if(stateFileDictionary[value] != null){
										$("#stateFiles li").eq(0).before("<li onclick='return loadStateFile(\""+value+"\", this, \"root\", \"text\")'>"+stateFileDictionary[value].description+"</li>");
									}
								}
							});
						}
					});
					
					$.ajax({
						url:"scenario",
						dataType:"json",
						data:{name:$("#manageScenario").val()},
						cache:false,
						success: function(data){
							$.each(data.files, function(index, value){
								var icon = "";
								if(data.file_edits[index]){
									icon = "<img src='/img/tick.png' />";
								}
								if(categoryDictionary[fileDictionary[value].category].name == "Settings"){
									$("#"+categoryDictionary[fileDictionary[value].category].div).append("<li onclick='return loadFile(\""+value+"\", this, \"text\")'> " +icon+ " "+fileDictionary[value].description+"</li>");
								}else{
									if(value=='phev_characteristics.csv'){
											$("#"+categoryDictionary[fileDictionary[value].category].div +" li").eq(7).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else if(value=='comm_service_fuel.csv'){	
											$("#"+categoryDictionary[fileDictionary[value].category].div + " li").eq(0).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else if(value=='other_ops.csv'){
											$("#"+categoryDictionary[fileDictionary[value].category].div + " li").eq(6).after("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
										}else{
											
									$("#"+categoryDictionary[fileDictionary[value].category].div).append("<li onclick='return loadFile(\""+value+"\", this)'> " +icon+ " "+fileDictionary[value].description+"</li>");
									}
								}
							});
							$("#scenarioChosen").html("Scenario " + $("#manageScenario").val() + " for "+ data.state + " selected");
						}
					});
				});
			});	
			
			function openReport(){
				$("#frame").css("display", "block");
				$("#frame").height($("#frame").contents().find("html").height());				
			}