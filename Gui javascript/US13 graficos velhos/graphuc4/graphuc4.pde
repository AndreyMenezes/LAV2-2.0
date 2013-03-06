class Bar{
 
  float mtt, mtp, ex, prova,total;
  int i,eixo_x,width_x,eixo_y;
  String id,nota;
  
  Bar(String id, float mtt, float mtp, float ex, float prova, int i, String nota){
    this.mtt = mtt;
    this.mtp = mtp;
    this.ex = ex;
    this.prova = prova;
    this.id = id;
    this.i = i;
    this.nota = nota;
    
    eixo_x = 20 + i*35;
    width_x = 50 + i*35;
    eixo_y = 563;
  }
  
  //padrao(70 + x*35,563,100 + x*35, nota*41 - 563)
  void display(){
    noStroke();
    rectMode(CORNERS);
    pushMatrix();

    fill(color(189,201,225));//mtt
    rect(eixo_x,eixo_y,width_x, (eixo_y - ((this.mtt+this.mtp+this.ex+this.prova)*41)));
    fill(color(103,169,207));//mtp
    rect(eixo_x,eixo_y,width_x, (eixo_y - ((this.mtp+this.ex+this.prova)*41)));
    fill(color(28,144,153));//provas
    rect(eixo_x,eixo_y,width_x, (eixo_y - ((this.ex+this.prova)*41)));
    fill(color(1,108,89));//exercicios
    rect(eixo_x,eixo_y,width_x, (eixo_y - ((this.ex)*41)));
    fill(102);
    
    
    rotate(0.457);
    text(id, (315 + i*32), (490 - i*16));
    popMatrix();

  //  NumberFormat formatter = new DecimalFormat("#0.0");
   // text(nota,eixo_x + 5 , (eixo_y - (float(nota)*42)));
  }
  
  void show_dados(){
    float max_nota = eixo_y - ((this.mtt+this.mtp+this.ex+this.prova)*41);

      if(mouseX >= eixo_x && mouseX <= width_x &&
       mouseY <= eixo_y && mouseY >= max_nota){
          String txt = "" + id;
          Label labels = new Label(txt,mouseX,mouseY,this.mtt, this.mtp,this.prova,this.ex);
    }
    
  }
    
}

class Label {
   
  Label(String txt, float x, float y, float mtt, float mtp, float prova,float ex) {
     
    // get text width
    float labelW = textWidth(txt);
//    NumberFormat formatter = new DecimalFormat("#0.0");
    
    // check if label would go beyond screen dims
    if (x + labelW + 150 > width) {
      x -= labelW + 130;
    }
     
    // draw bg
    image(img, x+10, y-30);
     
    //porcentagens

    // draw text
    fill(0);
    text(txt, x+65, y-5);
    text("Notas", x + 30, y+20);
    fill(color(189,201,225));//mtt
    rect(x + 125, y+ 165, x + 155, (y+165) - ((mtt+mtp+prova+ex)*17));
    rect(x + 160, y+ 165, x + 190, (y+165) - (9.2*17));
  //  text("MTT " + formatter.format((mtt+mtp+prova+ex)*mtt) + "%", x + 30, y+35);
    
    fill(color(103,169,207));//mtp
 //   text("MTP " + formatter.format((mtt+mtp+prova+ex)*mtp)+ "%", x+30, y+50);
    rect(x + 125, y+ 165, x + 155, (y+165) - ((mtp+prova+ex)*17));
    rect(x + 160, y+ 165, x + 190, (y+165) - (6.545*17));
    
    fill(color(28,144,153));
   // text("Provas " + formatter.format((mtt+mtp+prova+ex)*prova)+ "%", x+30, y+65);
    rect(x + 125, y+ 165, x + 155, (y+165) - ((prova+ex) *17));
    rect(x + 160, y+ 165, x + 190, (y+165) - (4.195*17));
    
    fill(color(1,108,89));
 //   text("Exerc. " + formatter.format((mtt+mtp+prova+ex)*ex)+ "%", x+30, y+80);
    rect(x + 125, y+ 165, x + 155, (y+165) - (ex*20));
    rect(x + 160, y+ 165, x + 190, (y+165) - (0.395*20));
    //pra essa base de dados o melhor aluno, automatizar isso depois
    //media.MTT media.MTP nota.listas media.provas 2.58      2.35       0.395         3.8
    fill(102);
  //  text(formatter.format(9.2),x+165, ((y+165) - ((9.2)*17.4)));
    
    
    
    
  }
}

  PImage bg,img;
  Bar[] bars = new Bar[110]; //110 eh a quantidade de alunos
  boolean flag = false;

void setup() {
  size(2000, 680);
  bg = loadImage("uc4.png");
  img = loadImage("label.png");
}

void draw() {
  background(255);
  fill(102);
  noStroke();
  text("Composição das notas dos alunos", 310, 70); 

  generateValues();
  bars[2] = new Bar("ideal",float("2.1"),float("1.75"),float("0.35"),float("2.8"),1, "7.0");
  bars[2].display();
  
  for(int i = 1; i < 110; i++){ 
    bars[i].show_dados();  
  }
}

void generateValues(){
 String[] data = loadStrings("uc4.csv");
//Bar(String id, float mtt, float mtp, float ex, float prova, int i)
  println(data.length);
  for(int i = 1; i < data.length; i++){
    String [] temp = split(data[i]," ");
    println(temp[1]);
   // println(float(temp[2]));
    bars[i] = new Bar(temp[1],float(temp[2]),float(temp[3]),float(temp[4]),float(temp[5]),i-1, (temp[2]+temp[3]+temp[4]+temp[5]));
    
    bars[i].display();  
  } 
 
}


