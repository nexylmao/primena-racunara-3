SrednjaVrednost = function(c)
{
	return(sum(c)/length(c));
}

Klasifikacija = function(df)
{
	df = df[order(df$X),]
	meds = c()
	for(i in seq(1,16,4))
	{
		med = mean(df$Y[i : (i + 3)])
		meds = c(meds, med)
	}
	return(meds)
}

ycoor = function(x)
{
	step = (max(x)-min(x))/10
	hello = c()
	for(i in seq(1,10,2))
	{
		hello = c(hello, min(x) + (i * step))
	}
	return(hello)
}

Regresija = function(x,y)
{
	b = (sum((x-SrednjaVrednost(x))*(y-SrednjaVrednost(y))))/(sum((x-SrednjaVrednost(x))^2))
	a = SrednjaVrednost(y) - (b * (SrednjaVrednost(x)))
	return(c(a,b))
}

StandardnaDevijacija = function(c)
{
	return(sqrt(sum((c-SrednjaVrednost(c))^2/(length(c)-1))))
}

Korelacija = function(x,y)
{
	return(sum((x-SrednjaVrednost(x))*(y-SrednjaVrednost(y)))/sqrt(sum((x-SrednjaVrednost(x))^2)*sum((y-SrednjaVrednost(y))^2)))
}