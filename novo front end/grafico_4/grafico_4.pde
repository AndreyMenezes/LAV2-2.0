/* 
import gui.GraphicUC;
*/
PImage bg;
PImage grid;
PImage logo;
String[] data;
int click;
int gx;
Bar[] allBars, bars;

 void setup() {
    size(1200,650);
    bg = loadImage("bg_uc4.png");
    grid = loadImage("grid2.png");
    data = loadStrings("uc4.csv");
    logo = loadImage("logo_bg.png");
    click = 0;
    gx = 63;/*
    bars = new Bar[5];
    generateValues();*/
 }
 
 //não adaptado
 void generateValues() {
    allBars = new Bar[data.length];
     
    for (int i = 1; i < 58; i++) {
      String[] temp= split(data[i],",");
      String id = temp[1];
      float mtt = Float.parseFloat(temp[2]);
      float mtp =  Float.parseFloat(temp[3]);
      float ex = Float.parseFloat(temp[4]);
      float prova =  Float.parseFloat(temp[5]);
      float nota_final =  Float.parseFloat(temp[6]);
      allBars[i] = new Bar(id,mtt,mtp,ex,prova, i,nota_final);
    }
  }
 
 void draw() {
    background(255);
    image(bg,0,100);
    image(logo, 1000,600);/*
    updategrid(click);
    showbars(click);*/
 }
 
  void updategrid(int click){
    
    if(click != 0)
      image(grid,gx + (click * 19), 481);
    else
      image(grid, gx, 481);
   }
   
   //não adaptado
   void showbars(int click) {
    for (int i = 0; i < bars.length; i++)
      bars[i] = getBar(click + (i + 1));
    
    //NumberFormat formatter = new DecimalFormat("#0.0");
    noStroke();
      
    //barra mtt = nota total
    fill(189,201,225);//mtt
    for (int i = 0; i < bars.length; i++)
      rect(195 + (i * 95), 440, 115 + (i * 95), 476 - (bars[i].nota) * 35);
      
    //barra mtp = nota - mtt
    fill(103,169,207);//mtp
    for (int i = 0; i < bars.length; i++)
      rect(195 + (i * 95), 440, 115 + (i * 95), 467 - (bars[i].nota - bars[i].mtt) * 35);
      
    //barra provas = nota - (mtt + mtp)
    fill(28,144,153);//provas
    for (int i = 0; i < bars.length; i++)
      rect(195 + (i * 95), 440, 115 + (i * 95), 460 - (bars[i].ex + bars[i].prova) * 35);
    
    //barra exercicios
    fill(1,108,89);
    for (int i = 0; i < bars.length; i++)
      rect(195 + (i * 95), 440, 115 + (i * 95), 445 - (bars[i].ex) * 35);
    
    fill(130,130,130);
    for (int i = 0; i < bars.length; i++) {
      text(bars[i].id, 120 + (i * 96), 460);
      text(formatter.format(bars[i].nota), 140 + (i * 96), (465 - (bars[i].nota) * 35));
    }
  }
  
  //não adaptado e precisa da classe Bar solução trazela para o arquivo de algum jeito
  Bar getBar(int i){
    return allBars[i];
  }
 
 
 void mousePressed() {
   //click = 0;
   if (overRect(575,265,49,60)) {
      if (click < 52) click += 1;
   }else if (overRect(63,265,49,60)) {
      if (click >= 1) click -= 1;
   }
 }
 
 boolean overRect(int x, int y, int width, int height)  {
    return (mouseX >= x && mouseX <= x+width && mouseY >= y && mouseY <= y+height);
 }

class Bar{
  
  float mtt, mtp, ex, prova,total, nota;
  int i,eixo_x,width_x,eixo_y;
  String id;
  
  Bar(String id, float mtt, float mtp, float ex, float prova, int i, float nota){
    this.mtt = mtt;
    this.mtp = mtp;
    this.ex = ex;
    this.prova = prova;
    this.id = id;
    this.i = i;
    this.nota = nota;
  }
  
  float getNota(){
    return this.nota;
  }
  
}

