package lat;

import gui.GraphicUC;

import java.util.ArrayList;
import java.util.List;

import processing.core.PApplet;
import processing.core.PImage;
import uc.UC1;
import uc.UC2;
import uc.UC3;
import uc.UC4;

public class LAT extends PApplet {
	
	private int rectSize = 90;
	private PImage logo,bg,grid,bg3_1,bg3_2,bg4,grid2;
	
	private List<GraphicUC> ucs;
	private int uc_atual;
	
	private int click = 0;
	
	@Override
	public void setup() {
		size(1200,650);
		//logo = parent.loadImage("logo_bg.png");
				
		ucs = new ArrayList<GraphicUC>();
		uc_atual = 0;
		//dados uc1
		this.bg = loadImage("bg_uc1.png");
		this.grid = loadImage("grid.png");
		String[] data1 = loadStrings("UC1_Frequencias_Prova1.csv");
		String[] data2 = loadStrings("UC1_Frequencias_Prova2.csv");
		String[] data3 = loadStrings("UC1_Frequencias_Prova3.csv");
		String[] data4 = loadStrings("UC1_Bayes_Prova1_Formatado.txt");
		
		ucs.add(new UC1(this,bg, grid,data1,data2,data3,data4));
		
		//dados uc2
		//StString[] ring lines[][] = loadStrings("correlacoes.csv");
		data1 = loadStrings("correlacoes.csv");
		data2 = loadStrings("UC2-Prova1.csv");
		data3 = loadStrings("UC2-Prova2.csv");
		data4 = loadStrings("UC2-Prova3.csv");
		ucs.add(new UC2(this,data1,data2,data3,data4));
		
		//dados uc3
		data1 = loadStrings("AgrupamentoAtividade.csv");
		data2 = loadStrings("AgrupamentoExercicios.csv");
		this.bg3_1 = loadImage("uc3.1.png");
		this.bg3_2 = loadImage("uc3.2.png");
		ucs.add(new UC3(this,bg3_1,bg3_2,data1,data2));
		//dados uc4
		this.bg4 = loadImage("bg_uc4.png");
		this.grid2 = loadImage("grid2.png");
		String[] dados = loadStrings("uc4.csv");
		ucs.add(new UC4(this,bg4,grid2,dados));
	}
	
	public void draw() {
		background(255);
		fill(245,245,245);
		rectMode(CORNERS);
		stroke(245,245,245);
		rect(0, 0, width, rectSize);
		  
		//Definindo os retangulos das opcoes
		  
		fill(240,240,240);
		rect(115,70,320,28);
		rect(340,70,555,28);
		rect(580,70,775,28);
		rect(800,70,1020,28);
		
		fill(252,252,252);
		
		fill(255);
		ucs.get(uc_atual).draw(click);
		fill(255);
		  
		rect(115,65,315,29);
		rect(340,65,550,29);
		rect(580,65,770,29);
		rect(810,65,1015,29);
		
		
		fill(130,130,130);
		text("Exercícios relevantes",155,50);
		text("Correlação Exercícios e Notas",350,50);
		text("Agrupamento",630,50);
		text("Composição das Notas",850,50);
	}
	
	@Override
	public void mousePressed() {
		
		ucs.get(uc_atual).mousePressed();
		
		if (overRect(115,30,200,30)) {
			click = 0;
			uc_atual = 0;
		} else if (overRect(340,30,210,30)) {
			click = 0;
			uc_atual = 1;
		} else if (overRect(580,30,200,30)) {
			uc_atual = 2;
		} else if (overRect(820,30,200,30)) {
			click = 0;
			uc_atual = 3;
		}
		
		if (uc_atual == 3 && overRect(575,265,49,60)) {
			if (click < 52) click += 1;
		} else if (uc_atual == 3 && overRect(63,265,49,60)) {
			if (click >= 1) click -= 1;
		}

		if (uc_atual == 1 & overRect(350,120,205,35))
			click = 0;
		else if (uc_atual == 1 & overRect(580,120,205,35))
			click = 1;
		else if (uc_atual == 1 & overRect(815,120,205,35))
			click = 2;
		
		if (uc_atual == 2 & overRect(350,120,205,35))
			click = 1;
		else if (uc_atual == 2 & overRect(580,120,205,35))
			click = 0;
	}
	
	public boolean overRect(int x, int y, int width, int height)  {
	  return (mouseX >= x && mouseX <= x+width && mouseY >= y && mouseY <= y+height);
	}
	
	 public static void main(String[ ] args) {
		 LAT t = new LAT();
		 t.setup();
		 t.draw();
		 
		 
	 }
}
