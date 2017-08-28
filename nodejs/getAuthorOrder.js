const path='../results/'
const csvFilePath = [] 
const authorList = [ 
/*angular*/{authors: ['Parin Kaewsim']},
/*bams*/{authors: ['Tosak Settharungson']},
/*black duck hub*/{authors: ['Wilasani Sang-Ime']},
/*compass time reporting*/{authors: ['Parin Kaewsim','Nattcha Raktasuvarna','Pisit Phaungsuk']},
/*customer exp*/{authors: ['Pornthep Seawlho','Robin Schribman']},
/*eikon desktop*/{authors: ['Anusorn Rochana-aree']},
/*gitlab*/{authors: ['Tosak Settharungson','Anattapol Limopasit','Pawita Jongsuebsuk']},
/*how can I track mt project*/{authors: ['Anna Stepanova']},
/*how to create a tr mobile*/{authors: ['Ronald Kozoman']},
/*I have an idea*/{authors: ['Anna Stepanova']},
/*innovation*/{authors: ['Anna Stepanova']},
/*iTracker*/{authors: ['Parin Kaewsim','Nattcha Raktasuvarna','Pisit Phaungsuk']},
/*JIRA F&R*/{authors: ['Orawan Atsawasrisilp','Phattaraphorn Suanses','Kantatorn  Tardthong']},
/*JIRA SSDF*/{authors: ['Orawan Atsawasrisilp','Phattaraphorn Suanses','Kantatorn  Tardthong']},
/*metrics database*/{authors: ['Wilasani Sang-Ime']},
/*PAAT*/{authors: ['Wilasani Sang-Ime']},
/*SAMI*/{authors: ['Tosak Settharungson','Anattapol Limopasit','Pawita Jongsuebsuk']},
/*Sonaqube*/{authors: ['Tosak Settharungson']},
/*SPE CI*/{authors: ['Dustin Wolfe']},
/*thehub*/{authors: ['Lindsay Keogh']},
/*veracode*/{authors: ['Jeremiah Cone']},
/*what are avilable tr ideation*/{authors: ['Anna Stepanova']},
/*what is our mobile app*/{authors: ['Ronald Kozoman']},
/*where can I raise my ideas*/{authors: ['Anna Stepanova']},
/*where can I share my ideas*/{authors: ['Anna Stepanova']},
/*where can I track my work*/{authors: ['Anna Stepanova']}
]
const csv = require('csvtojson')
const fs = require('fs');
var results = new Array(csvFilePath.length);

fs.readdirSync(path).forEach(file => {
    /*any file which is ended with -active.csv*/
    if(file.match("-active.csv$")){
        csvFilePath.push(path+file)
    }
})

var proms = []

console.log('reading.. '+ csvFilePath.length + ' files')
for(let i=0; i<csvFilePath.length; i++){
    console.log('reading..'+ csvFilePath[i])
    let prom = new Promise((resolve,reject) => {
        let csvObj = []
        csv()
        .fromFile(csvFilePath[i])
        .on('json',(jsonObj)=>{
            csvObj.push(jsonObj)
        })
        .on('done',(error)=>{
            if(!error){
                resolve(csvObj)
            }else{
                reject('convert from csv error')
            }
        })
    })
    .then( (csvObj) => {
        console.log('then..')
        console.log('read success!')
        console.log('searching for.. '+authorList[i].authors)
        let found = false
        for(var j = 0; j < csvObj.length; j++) {
            if (!found) {
                authorList[i].authors.forEach( function(a) {
                    if(!found && a.indexOf(csvObj[j].author.name) != -1){
                        results[i] = j
                        found = true
                    }
                })
            }
        }
        //resolve('author finding success!')
    }, (error) => {console.log(error)} );
    proms.push(prom)
    console.log('push promise!')
}

Promise.all(proms).then(
    (success) => {
        console.log('then..')
        console.log("all prom success")
        let str = ""
        for(let i=0; i<csvFilePath.length; i++){
            if(results[i]||results[i]==0){
                str += csvFilePath[i]+" : "+results[i]+"\n"
            }else{
                str += csvFilePath[i]+" : -1\n"
            }
        }
        
        fs.writeFile(path+'results.txt', str, function(err) {
            if(err) {
                return console.log(err);
            }
        
            console.log("The file was saved at "+path+'results.txt');
        }); 
    }
)